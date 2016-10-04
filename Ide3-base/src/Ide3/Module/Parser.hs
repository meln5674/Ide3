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
module Ide3.Module.Parser where

import Data.Monoid

import Control.Monad

import Language.Haskell.Exts.Annotated.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Parser
    ( ParseResult(..)
    , defaultParseMode
    , parseFilename
    , extensions
    , fixities
    )
import Language.Haskell.Exts (readExtensions)
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc (SrcInfo)
import Language.Haskell.Exts.Comments
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax

import Ide3.Types.Internal

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Export.Parser as Export
import qualified Ide3.Import.Parser as Import
import Ide3.SrcLoc

import Ide3.Utils.Parser


-- | Results of extracting information from the third-party parser
data ExtractionResults l
    = Extracted (Ann l ModuleInfo)
                (Ann l String)
                [(Ann l Pragma)]
                (Maybe [Ann l (WithBody Export)])
                [Ann l (WithBody Import)]
                [Ann l (WithBody Declaration)]

annotate :: FileSpannable l => (a -> l) -> (a -> x) -> a -> Ann SrcFileSpan x
annotate getAnn convert a = Ann (toSrcFileSpan $ getAnn a) (convert a)

-- | Extract identifying information about a module
extractInfo :: (SrcInfo l, FileSpannable l) 
            => String 
            -> (Syntax.Module l, [Comment]) 
            -> Ann SrcFileSpan ModuleInfo
extractInfo _ (Syntax.Module _ (Just mHead) _ _ _, _) = Ann mHeadAnn' mInfo
  where
    (Syntax.ModuleHead mHeadAnn mName _ _) = mHead 
    (Syntax.ModuleName _ name) = mName
    mInfo = ModuleInfo $ Symbol name
    mHeadAnn' = toSrcFileSpan mHeadAnn
extractInfo _ (m,_) = Ann mAnn' mInfo
  where
    mAnn = Syntax.ann m
    mAnn' = mkSrcFileSpanFrom mAnn noSpan
    mInfo = UnamedModule Nothing

-- | Extract the pragmas from a module
extractPragmas :: (SrcInfo l, FileSpannable l)
               => String 
               -> (Syntax.Module l, [Comment]) 
               -> [(Ann SrcFileSpan Pragma)]
extractPragmas _ (Syntax.Module _ _ ps _ _,_) = map (annotate Syntax.ann prettyPrint) ps
  where
extractPragmas _ _ = []

-- | Extract the header from a module
extractHeader :: (SrcInfo l, FileSpannable l) 
              => String -> (Syntax.Module l, [Comment]) 
              -> Ann SrcFileSpan String
extractHeader s (Syntax.Module mspan maybeHeader _ _ _,comments) = case headerComments of
    [] -> Ann (mkSrcFileSpanFrom mspan noSpan) ""
    _ -> Ann (mkSrcFileSpanFrom mspan $ SrcSpan headerStart headerEnd) $ headerSpan >< s
  where
    headerComments = flip filter comments $ \(Comment _ cspan _) -> getSpanEnd cspan < moduleStart
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
    = Just $ map (annotate Syntax.ann $ Export.convertWithBody str) exports 
  where
extractExports _ _ = Nothing

-- | Extract the imports from a module
extractImports :: FileSpannable l 
               => String 
               -> (Syntax.Module l, [Comment]) 
               -> [Ann SrcFileSpan (WithBody Import)]
extractImports str m
    | (Syntax.Module _ _ _ imports _, _) <- m
    = map (annotate Syntax.ann $ Import.convertWithBody str) imports
  where
extractImports _ _ = []


-- | Extract the declarations from a module
extractDecls :: (SrcInfo l, FileSpannable l) 
             => String 
             -> (Syntax.Module l, [Comment]) 
             -> Either (SolutionError u) [Ann SrcFileSpan (WithBody Declaration)]
extractDecls str (Syntax.Module mAnn _ _ _ decls, cs) = do
    converted <- forM decls $ \d -> do
        Declaration.convertWithBody str cs d
    let pathless = Declaration.combineMany converted
    return $ map (mapAnn $ mkSrcFileSpanFrom mAnn) pathless
extractDecls _ _ = Right []

-- | Extract the data needed for buliding a Module
extract :: (SrcInfo l, FileSpannable l)
        => String 
        -> (Syntax.Module l, [Comment]) 
        -> Either (SolutionError u) (ExtractionResults SrcFileSpan)
extract str x = do
    let info    =  extractInfo      str x
        pragmas =  extractPragmas   str x
        header  =  extractHeader    str x
        exports =  extractExports   str x
        imports =  extractImports   str x
    decls       <- extractDecls     str x
    return $ Extracted info header pragmas exports imports decls

-- | Take a string and produce the needed information for building a Module
parse :: String 
      -> Maybe FilePath 
      -> Either (SolutionError u) (ExtractionResults SrcFileSpan)
parse s p = case parseModuleWithComments parseMode s of
    ParseOk x -> extract s x
    ParseFailed l msg -> Left $ ParseError (toSrcFileLoc l) msg ""
  where
    exts = maybe [] snd $ readExtensions s
    parseMode = case p of
        Just fn -> defaultParseMode
                 { parseFilename=fn
                 , extensions= EnableExtension MultiParamTypeClasses : exts
                 , fixities=Just[]
                 }
        Nothing -> defaultParseMode
                 { extensions= EnableExtension MultiParamTypeClasses : exts
                 , fixities=Just[]
                 }

-- | Parse a module and assume its name is "Main" if it doesn't already have one
parseMain :: String 
          -> Maybe FilePath 
          -> Either (SolutionError u) (ExtractionResults SrcFileSpan)
parseMain s p = case parseModuleWithComments parseMode s of
    ParseOk x -> do
        Extracted i h ps es is ds <- extract s x
        case i of
            Ann l (UnamedModule _) -> return 
                            $ Extracted (Ann l $ ModuleInfo (Symbol "Main")) h ps es is ds
            info -> return $ Extracted info h ps es is ds
    ParseFailed l msg -> Left $ ParseError (toSrcFileLoc l) msg ""
  where
    exts = (maybe [] snd $ readExtensions s)
    parseMode = case p of
        Just fn -> defaultParseMode
                 { parseFilename=fn
                    -- temporary workaround for https://github.com/haskell-suite/haskell-src-exts/issues/304
                 , extensions = EnableExtension MultiParamTypeClasses : exts
                 , fixities = Just[]
                 }
        Nothing -> defaultParseMode
                 { extensions = EnableExtension MultiParamTypeClasses : exts
                 , fixities = Just[]
                 }


parseAtLocation :: [SrcLoc]
                -> String
                -> Maybe FilePath
                -> Either (SolutionError u) [Maybe (ModuleItem, SrcLoc)]
parseAtLocation ls s p = do
    Extracted _ hc ps es is ds <- Ide3.Module.Parser.parse s p
    let try :: (x -> ModuleItem) -> SrcLoc -> Ann SrcFileSpan x -> Maybe (ModuleItem, SrcLoc)
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

