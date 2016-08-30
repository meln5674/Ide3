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
module Ide3.Module.Parser 
    ( module Ide3.Module.Parser
    , SrcSpanInfo
    ) where

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
import Language.Haskell.Exts.SrcLoc
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

-- | Extract identifying information about a module
extractInfo :: SrcInfo l => String -> (Syntax.Module l, [Comment]) -> Ann l ModuleInfo
extractInfo _ (Syntax.Module l' (Just (Syntax.ModuleHead l (Syntax.ModuleName _ n) _ _)) _ _ _, _) = Ann l $ ModuleInfo (Symbol n)
extractInfo _ _ = Ann (toSrcInfo noLoc [] noLoc) $ UnamedModule Nothing

-- | Extract the pragmas from a module
extractPragmas :: SrcInfo l => String -> (Syntax.Module l, [Comment]) -> [(Ann l Pragma)]
extractPragmas _ (Syntax.Module _ _ ps _ _,_) = flip map ps $ \p -> Ann (Syntax.ann p) (prettyPrint p)
extractPragmas _ _ = []

-- | Extract the header from a module
extractHeader :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> Ann SrcSpanInfo String
extractHeader s (Syntax.Module mspan maybeHeader _ _ _,comments) = case headerComments of
    [] -> Ann (toSrcInfo noLoc [] noLoc) ""
    _ -> Ann (toSrcInfo headerStart [] headerEnd) $ headerSpan >< s
  where
    headerComments = flip filter comments $ \(Comment _ cspan _) -> cspan < moduleStart
    moduleStart = case maybeHeader of
        Just header -> srcInfoSpan $ Syntax.ann header
        Nothing -> head (srcInfoPoints mspan)
    Comment _ firstSpan _ = head headerComments
    Comment _ lastSpan _ = last headerComments
    headerStart = SrcLoc
                { srcFilename = srcSpanFilename firstSpan
                , srcLine = srcSpanStartLine firstSpan
                , srcColumn = srcSpanStartColumn firstSpan
                }
    headerEnd = SrcLoc
                { srcFilename = srcSpanFilename lastSpan
                , srcLine = srcSpanEndLine lastSpan
                , srcColumn = srcSpanEndColumn lastSpan
                }
    headerSpan = mkSrcSpan headerStart headerEnd
extractHeader _ _ = Ann (toSrcInfo noLoc [] noLoc) ""

-- | Extract the exports from a module
extractExports :: Spannable l => String -> (Syntax.Module l, [Comment]) -> Maybe [Ann l (WithBody Export)]
extractExports str (Syntax.Module _ (Just (Syntax.ModuleHead _ _ _ (Just (Syntax.ExportSpecList _ exports)))) _ _ _, _)
     = Just $ flip map exports $ \e -> Ann (Syntax.ann e) (Export.convertWithBody str e)
extractExports _ _ = Nothing

-- | Extract the imports from a module
extractImports :: Spannable l => String -> (Syntax.Module l, [Comment]) -> [Ann l (WithBody Import)]
extractImports str (Syntax.Module _ _ _ imports _, _)
    = flip map imports $ \i -> Ann (Syntax.ann i) (Import.convertWithBody str i)
extractImports _ _ = []

-- | Extract the declarations from a module
extractDecls :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> Either (SolutionError u) [Ann SrcSpanInfo (WithBody Declaration)]
extractDecls str (Syntax.Module _ _ _ _ decls, cs)
    = liftM Declaration.combineMany $ forM decls $ \d -> do
        Declaration.convertWithBody str cs d
extractDecls _ _ = Right []

-- | Extract the data needed for buliding a Module
extract :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> Either (SolutionError u) (ExtractionResults SrcSpanInfo)
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
      -> Either (SolutionError u) (ExtractionResults SrcSpanInfo)
parse s p = case parseModuleWithComments parseMode s of
    ParseOk x -> extract s x
    ParseFailed l msg -> Left $ ParseError l msg ""
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
          -> Either (SolutionError u) (ExtractionResults SrcSpanInfo)
parseMain s p = case parseModuleWithComments parseMode s of
    ParseOk x -> do
        Extracted i h ps es is ds <- extract s x
        case i of
            Ann l (UnamedModule _) -> return 
                            $ Extracted (Ann l $ ModuleInfo (Symbol "Main")) h ps es is ds
            info -> return $ Extracted info h ps es is ds
    ParseFailed l msg -> Left $ ParseError l msg ""
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


parseAtLocation :: (Int,Int)
                -> String
                -> Maybe FilePath
                -> Either (SolutionError u) (Maybe (ModuleItem, Int, Int))
parseAtLocation (r,c) s p = do
    let l = SrcLoc (maybe "" id p) r c
    Extracted _ hc ps es is ds <- Ide3.Module.Parser.parse s p
    let tryHeader = case () of
            ()
                | (ann hc) `contains` l -> Just (mitem, r', c')
                | otherwise -> Nothing
                  where
                    mitem = HeaderCommentItem $ unAnn hc
                    r' = r - startLine (ann hc) 
                    c' = c
        tryPragmas = flip map ps $ \p -> case () of
            ()
                | ann p `contains` l -> Just (mitem, r', c')
                | otherwise -> Nothing
                  where
                    mitem = PragmaItem $ unAnn p
                    r' = r - startLine (ann p)
                    c' = c
        tryExports = case es of
            Just es -> flip map es $ \e -> case () of
                ()
                    | ann e `contains` l -> Just (mitem, r', c')
                    | otherwise -> Nothing        
                      where
                        mitem = ExportItem $ unAnn e
                        r' = r - startLine (ann e)
                        c' = c
            Nothing -> []
        tryDeclarations = flip map ds $ \d -> case () of
            ()
                | ann d `contains` l -> Just (mitem, r', c')
                | otherwise -> Nothing
                  where
                    mitem = DeclarationItem $ unAnn d
                    r' = r - startLine (ann d)
                    c' = c
    
    return $ getFirst 
           $ mconcat 
           $ map First 
           $  tryHeader 
           :  tryPragmas
           ++ tryExports
           ++ tryDeclarations

