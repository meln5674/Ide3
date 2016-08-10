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

-- | Results of extracting information from the third-party parser
data ExtractionResults
    = Extracted ModuleInfo
                String
                [Pragma]
                (Maybe [WithBody Export])
                [WithBody Import]
                [WithBody Declaration] 

-- | Extract identifying information about a module
extractInfo :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> ModuleInfo
extractInfo _ (Syntax.Module _ (Just (Syntax.ModuleHead _ (Syntax.ModuleName _ n) _ _)) _ _ _, _) = ModuleInfo (Symbol n)
extractInfo _ _ = UnamedModule Nothing

-- | Extract the pragmas from a module
extractPragmas :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> [Pragma]
extractPragmas _ (Syntax.Module _ _ ps _ _,_) = map prettyPrint ps
extractPragmas _ _ = []

-- | Extract the header from a module
extractHeader :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> String
extractHeader s (Syntax.Module mspan maybeHeader _ _ _,comments) = case headerComments of
    [] -> ""
    _ -> headerSpan >< s
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
extractHeader _ _ = ""

-- | Extract the exports from a module
extractExports :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> Maybe [WithBody Export]
extractExports str (Syntax.Module _ (Just (Syntax.ModuleHead _ _ _ (Just (Syntax.ExportSpecList _ exports)))) _ _ _, _)
     = Just $ map (Export.convertWithBody str) exports
extractExports _ _ = Nothing

-- | Extract the imports from a module
extractImports :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> [WithBody Import]
extractImports str (Syntax.Module _ _ _ imports _, _)
    = map (Import.convertWithBody str) imports
extractImports _ _ = []

-- | Extract the declarations from a module
extractDecls :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> Either (SolutionError u) [WithBody Declaration]
extractDecls str (Syntax.Module _ _ _ _ decls, cs)
    = Declaration.combineMany <$> mapM (Declaration.convertWithBody str cs) decls
extractDecls _ _ = Right []

-- | Extract the data needed for buliding a Module
extract :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> Either (SolutionError u) ExtractionResults
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
      -> Either (SolutionError u) ExtractionResults
parse s p = case parseModuleWithComments parseMode s of
    ParseOk x -> extract s x
    ParseFailed l msg -> Left $ ParseError l msg ""
  where
    exts = maybe [] snd $ readExtensions s
    parseMode = case p of
        Just fn -> defaultParseMode
                 { parseFilename=fn
                 , extensions=exts
                 , fixities=Just[]
                 }
        Nothing -> defaultParseMode
                 { extensions=exts
                 , fixities=Just[]
                 }

-- | Parse a module and assume its name is "Main" if it doesn't already have one
parseMain :: String 
          -> Maybe FilePath 
          -> Either (SolutionError u) ExtractionResults
parseMain s p = case parseModuleWithComments parseMode s of
    ParseOk x -> do
        Extracted i h ps es is ds <- extract s x
        case i of
            UnamedModule _ -> return 
                            $ Extracted (ModuleInfo (Symbol "Main")) h ps es is ds
            info -> return $ Extracted info h ps es is ds
    ParseFailed l msg -> Left $ ParseError l msg ""
  where
    exts = (maybe [] snd $ readExtensions s)
    parseMode = case p of
        Just fn -> defaultParseMode
                 { parseFilename=fn
                 , extensions=exts
                 , fixities=Just[]
                 }
        Nothing -> defaultParseMode
                 { extensions=exts
                 , fixities=Just[]
                 }
