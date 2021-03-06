{-|
Module      : Ide3.Import.Parser
Description : Parsing imports
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

module Ide3.Import.Parser where

import Data.Text (Text)
import qualified Data.Text as T

import Language.Haskell.Exts.Parser hiding (parse)
import Language.Haskell.Exts.Syntax hiding (Symbol, Module)


import Ide3.Types.Internal hiding (body)
import Ide3.Types.Exts()

import Ide3.SrcLoc
import Ide3.SrcLoc.Exts()

-- | Convert from the third party import type
convert :: ImportDecl a -> Import
convert x = case importSpecs x of
    Nothing -> ModuleImport sym isQualified rename
    Just (ImportSpecList _ True ss)
        -> BlacklistImport sym isQualified rename (map getSpec ss)
    Just (ImportSpecList _ False ss) 
        -> WhitelistImport sym isQualified rename (map getSpec ss)
  where
    ModuleName _ n = importModule x
    sym = Symbol $ T.pack n
    rename = case importAs x of
        Just (ModuleName _ n') -> Just $ Symbol $ T.pack n'
        Nothing -> Nothing
    isQualified = importQualified x

-- | Convert from the third part import type and extract the body as well
convertWithBody :: (Spannable a) => Text -> ImportDecl a -> WithBody Import
convertWithBody str x = WithBody import_ body
  where
    body = ann x >< str
    import_ = convert x

-- | Parse an import statement
parse :: Text -> Either (SolutionError u) Import
parse s = case parseImportDecl (T.unpack s) of
    ParseOk x -> Right $ convert x
    ParseFailed l msg -> Left $ ParseError (toSrcFileLoc l) msg ""

-- | Convert a third party import kind
getSpec :: ImportSpec a -> ImportKind
getSpec (IVar _ n) = NameImport (toSym n)
getSpec (IAbs _ (NoNamespace _) n) = NameImport (toSym n)
getSpec (IThingAll _ n) = AggregateImport (toSym n) Nothing
getSpec (IThingWith _ n ns) = AggregateImport (toSym n) $ Just $ map toSym ns
getSpec _ = error "Invalid import"
