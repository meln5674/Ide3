{-|
Module      : Ide3.Export.Parser
Description : Parsing exports
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides functions for parsing exports
-}
module Ide3.Export.Parser where

import Language.Haskell.Exts.Parser (ParseResult(..))
import Language.Haskell.Exts.Annotated.Parser hiding (parse)
import qualified Language.Haskell.Exts.Annotated.Parser as Parser
import Language.Haskell.Exts.Annotated.Syntax hiding (Module)
import Language.Haskell.Exts.SrcLoc

import Ide3.Types
import Ide3.SrcLoc


-- | Convert from the third party export
convert :: ExportSpec a -> Export
convert export = case export of
    EVar _ n -> SingleExport (toSym n)
    EAbs _ (NoNamespace _) n -> SingleExport (toSym n)
    --EAbs ?? n
    EThingAll _ n -> AggregateExport (toSym n) Nothing
    EThingWith _ n ns -> AggregateExport (toSym n) (Just $ map toSym ns)
    EModuleContents _ n -> ModuleExport (toSym n)

-- | Convert from the third party export and extract the body
convertWithBody :: Spanable a => String -> ExportSpec a -> WithBody Export
convertWithBody str export = WithBody export' body 
  where
    export' = convert export
    body = ann export >< str

-- | Parse an export
parse :: String -> Either String Export
parse s = case result of
    ParseOk ok -> Right $ convert export
      where 
        headAndImports = unNonGreedy ok
        ModuleHeadAndImports _ _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ exportList)))) _ = headAndImports
        [export] = exportList
    ParseFailed _ s -> Left s
  where
    dummyHeader = "module DUMMY (" ++ s ++ ") where"
    result = Parser.parse dummyHeader :: (ParseResult (NonGreedy (ModuleHeadAndImports SrcSpanInfo)))