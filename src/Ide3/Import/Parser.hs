{-|
Module      : Ide3.Import.Parser
Description : Parsing imports
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides functions to parse import statements
-}
module Ide3.Import.Parser where

import Language.Haskell.Exts.Annotated.Parser
import Language.Haskell.Exts.Parser (ParseResult(..))
import Language.Haskell.Exts.Annotated.Syntax hiding (Symbol, Module)


import Ide3.Types hiding (body)
import Ide3.SrcLoc

-- | Convert from the third party import type
convert :: Show a => ImportDecl a -> Import
convert x = case importSpecs x of
    Nothing -> ModuleImport sym isQualified rename
    Just (ImportSpecList _ True ss) -> BlacklistImport sym isQualified rename (map getSpec ss)
    Just (ImportSpecList _ False ss) -> WhitelistImport sym isQualified rename (map getSpec ss)
  where
    ModuleName _ n = importModule x
    sym = Symbol n
    rename = case importAs x of
        Just (ModuleName _ n') -> Just (Symbol n')
        Nothing -> Nothing
    isQualified = importQualified x

-- | Convert from the third part import type and extract the body as well
convertWithBody :: (Spanable a,Show a) => String -> ImportDecl a -> WithBody Import
convertWithBody str x = WithBody import_ body
  where
    body = ann x >< str
    import_ = convert x

-- | Parse an import statement
parse :: String -> Either String Import
parse s = case parseImportDecl s of
    ParseOk x -> Right $ convert x
    ParseFailed _ msg -> Left msg

-- | Convert a third party import kind
getSpec :: Show a => ImportSpec a -> ImportKind
getSpec (IVar _ n) = NameImport (toSym n)
getSpec (IAbs _ (NoNamespace _) n) = NameImport (toSym n)
getSpec (IThingAll _ n) = AllImport (toSym n)
getSpec (IThingWith _ n ns) = SomeImport (toSym n) (map toSym ns)
getSpec x = error $ show x
