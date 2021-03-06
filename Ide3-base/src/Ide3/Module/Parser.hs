{-|
Module      : Ide3.Module.Parser
Description : Parsing Modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide3.Module.Parser where

import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts (readExtensions)
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc (SrcInfo, SrcSpanInfo)
import Language.Haskell.Exts.Comments
import qualified Language.Haskell.Exts.Syntax as Syntax

import Ide3.Types.Internal

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Export.Parser as Export
import qualified Ide3.Import.Parser as Import
import Ide3.SrcLoc

import Ide3.Utils.Parser


-- | Results of extracting information from the third-party parser
data ExtractionResults l
    = Extracted 
    { extractedModuleInfo :: Ann l ModuleInfo
    , extractedModuleHeader :: Ann l Text
    , exrtractedPragmas :: [Ann l Pragma]
    , extractedExports :: Maybe [Ann l (WithBody Export)]
    , extractedImports :: [Ann l (WithBody Import)]
    , extractedDeclarations :: [Ann l (WithBody Declaration)]
    }
    | Unparsable
    { unparsableErrorLocation :: SrcLoc
    , unparsableErrorMessage :: String
    , unparsableModuleInfo :: ModuleInfo
    , unparsableModuleContents :: Text
    }

-- | Create an annotated item by applying an annotation extration and item
-- extraction function
annotate :: FileSpannable l => (a -> l) -> (a -> x) -> a -> Ann SrcFileSpan x
annotate getAnn convert a = Ann (toSrcFileSpan $ getAnn a) (convert a)

-- | Extract identifying information about a module
extractInfo :: (FileSpannable l) 
            => String 
            -> (Syntax.Module l, [Comment]) 
            -> Maybe ModuleInfo
            -> Maybe (Ann SrcFileSpan ModuleInfo)
extractInfo _ (Syntax.Module _ (Just mHead) _ _ _, _) _ = Just $ Ann mHeadAnn' mInfo
  where
    (Syntax.ModuleHead mHeadAnn mName _ _) = mHead 
    (Syntax.ModuleName _ name) = mName
    mInfo = ModuleInfo $ Symbol $ T.pack name
    mHeadAnn' = toSrcFileSpan mHeadAnn
extractInfo _ (m,_) (Just mi) = Just $ Ann mAnn' mi
  where
    mAnn = Syntax.ann m
    mAnn' = mkSrcFileSpanFrom mAnn noSpan
extractInfo _ _ _ = Nothing

-- | Extract the pragmas from a module
extractPragmas :: (FileSpannable l)
               => String 
               -> (Syntax.Module l, [Comment]) 
               -> [Ann SrcFileSpan Pragma]
extractPragmas _ m
    | (Syntax.Module _ _ ps _ _,_) <- m 
    = map (annotate Syntax.ann (T.pack . prettyPrint)) ps
extractPragmas _ _ = []

-- | Extract the header from a module
extractHeader :: (FileSpannable l) 
              => String 
              -> (Syntax.Module l, [Comment]) 
              -> Ann SrcFileSpan Text
extractHeader s (Syntax.Module mspan maybeHeader _ _ _,comments) =
    case headerComments of
        [] -> Ann (mkSrcFileSpanFrom mspan noSpan) ""
        _ -> Ann (mkSrcFileSpanFrom mspan headerSpan) $ headerSpan >< T.pack s
  where
    headerComments = filter ((< moduleStart) . getSpanEnd . toSrcSpan) comments
    moduleStart = spanStart $ toSrcSpan $ maybe mspan Syntax.ann maybeHeader 
    Comment _ firstSpan _ = head headerComments
    Comment _ lastSpan _ = last headerComments
    headerStart = spanStart $ toSrcSpan firstSpan 
    headerEnd = spanEnd $ toSrcSpan lastSpan
    headerSpan = SrcSpan headerStart headerEnd
extractHeader _ (m,_) = Ann (mkSrcFileSpanFrom (Syntax.ann m) noSpan) ""

-- | Extract the exports from a module
extractExports :: FileSpannable l 
               => String 
               -> (Syntax.Module l, [Comment]) 
               -> Maybe [Ann SrcFileSpan (WithBody Export)]
extractExports str m 
    | (Syntax.Module _ (Just mHead) _ _ _, _)  <- m
    , (Syntax.ModuleHead _ _ _ (Just specList)) <- mHead 
    , (Syntax.ExportSpecList _ exports) <- specList 
    = Just $ map (annotate Syntax.ann $ Export.convertWithBody $ T.pack str) exports 
extractExports _ _ = Nothing

-- | Extract the imports from a module
extractImports :: FileSpannable l 
               => String 
               -> (Syntax.Module l, [Comment]) 
               -> [Ann SrcFileSpan (WithBody Import)]
extractImports str m
    | (Syntax.Module _ _ _ imports _, _) <- m
    = map (annotate Syntax.ann $ Import.convertWithBody $ T.pack str) imports
extractImports _ _ = []


-- | Extract the declarations from a module
extractDecls :: (SrcInfo l, FileSpannable l) 
             => String 
             -> (Syntax.Module l, [Comment]) 
             -> Either (SolutionError u) 
                       [Ann SrcFileSpan (WithBody Declaration)]
extractDecls str (Syntax.Module mAnn _ _ _ decls, cs) = do
    converted <- forM decls $ Declaration.convertWithBody (T.pack str) cs
    let pathless = Declaration.combineMany converted
    return $ map (mapAnn $ mkSrcFileSpanFrom mAnn) pathless
extractDecls _ _ = Right []

-- | Extract the data needed for buliding a Module
extract :: (SrcInfo l, FileSpannable l)
        => String 
        -> (Syntax.Module l, [Comment]) 
        -> Maybe ModuleInfo
        -> Either (SolutionError u) (ExtractionResults SrcFileSpan)
extract str x oldMi = do
    let maybeInfo    =  extractInfo      str x oldMi
        pragmas =  extractPragmas   str x
        header  =  extractHeader    str x
        exports =  extractExports   str x
        imports =  extractImports   str x
    decls       <- extractDecls     str x
    case maybeInfo of
        Just info -> return $ Extracted info header pragmas exports imports decls
        Nothing -> Left $ InternalError "Nameless module found without backup" ""


-- | Take a string and produce the needed information for building a Module
parse :: String 
      -> Maybe FilePath
      -> Maybe ModuleInfo
      -> Either (SolutionError u) (ExtractionResults SrcFileSpan)
parse s p oldMi = case parseModuleWithComments parseMode s of
    ParseOk x -> extract s x oldMi
    ParseFailed l msg -> case parseWithMode parseMode s of
        ParseOk (NonGreedy (PragmasAndModuleName l' _ maybeName)) -> case maybeName of
            Just name -> do
                let (Syntax.ModuleName _ mName) = name
                    mi = ModuleInfo $ Symbol $ T.pack mName
                    _ = l' :: SrcSpanInfo
                return $ Unparsable (toSrcLoc l) msg mi $ T.pack s
            Nothing -> Left $ InternalError "Unparsable module found without backup module info" ""
        ParseFailed l' msg' -> Left $ ParseError (toSrcFileLoc l') msg' ""
  where
    exts = maybe [] snd $ readExtensions s
    parseMode = case p of
        Just fn -> defaultParseMode
                 { parseFilename = fn
                 -- temporary workaround for 
                 -- https://github.com/haskell-suite/haskell-src-exts/issues/304
                 , extensions = EnableExtension MultiParamTypeClasses 
                              : EnableExtension FlexibleContexts
                              : exts
                 , fixities=Just[]
                 }
        Nothing -> defaultParseMode
                 { fixities=Just[]
                 -- temporary workaround for 
                 -- https://github.com/haskell-suite/haskell-src-exts/issues/304
                 , extensions = EnableExtension MultiParamTypeClasses 
                              : EnableExtension FlexibleContexts
                              : exts
                 }

-- | Parse a module and assume its name is "Main" if it doesn't already have one
parseMain :: String 
          -> Maybe FilePath 
          -> Either (SolutionError u) (ExtractionResults SrcFileSpan)
parseMain s p = case parseModuleWithComments parseMode s of
    ParseOk x -> extract s x (Just $ ModuleInfo $ Symbol "Main")
    ParseFailed l msg -> case parseWithMode parseMode s of
        ParseOk (NonGreedy (PragmasAndModuleName l' _ maybeName)) -> do
            let mName
                    | Just (Syntax.ModuleName _ justName) <- maybeName = justName
                    | otherwise = "Main"
                mi = ModuleInfo $ Symbol $ T.pack mName
                _ = l' :: SrcSpanInfo
            return $ Unparsable (toSrcLoc l) msg mi $ T.pack s
        ParseFailed l' msg' -> Left $ ParseError (toSrcFileLoc l') msg' ""
  where
    exts = maybe [] snd $ readExtensions s
    parseMode = case p of
        Just fn -> defaultParseMode
                 { parseFilename=fn
                 -- temporary workaround for 
                 -- https://github.com/haskell-suite/haskell-src-exts/issues/304
                 , extensions = EnableExtension MultiParamTypeClasses 
                              : EnableExtension FlexibleContexts
                              : exts
                 , fixities = Just[]
                 }
        Nothing -> defaultParseMode
                 { fixities = Just[]
                 -- temporary workaround for 
                 -- https://github.com/haskell-suite/haskell-src-exts/issues/304
                 , extensions = EnableExtension MultiParamTypeClasses 
                              : EnableExtension FlexibleContexts
                              : exts
                 }

-- | Parse a string of haskell code, extracting the items at specific locations
parseAtLocation :: [SrcLoc]
                -> String
                -> Maybe FilePath
                -> Maybe ModuleInfo
                -> Either (SolutionError u) [Maybe (ModuleItem, SrcLoc)]
parseAtLocation ls s p oldMi = do
    result <- Ide3.Module.Parser.parse s p oldMi
    case result of
        Extracted _ hc ps es is ds  -> do
            let try :: (x -> ModuleItem) 
                    -> SrcLoc 
                    -> Ann SrcFileSpan x 
                    -> Maybe (ModuleItem, SrcLoc)
                try mkItem l x = case () of
                    ()
                        | xSpan `contains` l -> Just (mItem, l')
                        | otherwise -> Nothing
                          where
                            xSpan = ann x
                            mItem = mkItem $ unAnn x
                            l' = l `diffRows` getSpanStart xSpan
                tryHeader l = try HeaderCommentItem l hc
                tryPragmas l = map (try PragmaItem l) ps
                tryExports l = maybe [] (map $ try ExportItem l) es
                tryImports l = map (try ImportItem l) is
                tryDeclarations l = map (try DeclarationItem l) ds
                tryAll l = getFirst 
                    $ mconcat 
                    $ map First 
                    $  tryHeader l
                    :  tryPragmas l
                    ++ tryExports l
                    ++ tryImports l
                    ++ tryDeclarations l
            return $ map tryAll ls
        _ -> return []
