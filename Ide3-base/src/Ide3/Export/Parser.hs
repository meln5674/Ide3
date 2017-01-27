{-|
Module      : Ide3.Export.Parser
Description : Parsing exports
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
module Ide3.Export.Parser (parse, convertWithBody) where

import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Language.Haskell.Exts.Parser hiding (parse)
import qualified Language.Haskell.Exts.Parser as Parser
import Language.Haskell.Exts.Syntax hiding (Module)
import Language.Haskell.Exts.SrcLoc

import Ide3.Types.Exts()
import Ide3.Types.Internal
import Ide3.SrcLoc

import Ide3.SrcLoc.Exts()


-- | Convert from the third party export
convert :: ExportSpec a -> Export
convert export = case export of
    EVar _ n -> SingleExport (toSym n)
    EAbs _ (NoNamespace _) n -> SingleExport (toSym n)
    EAbs{} -> error "FOUND AN ABS EXPORT"

    EThingWith _ (EWildcard _ 0) n [] -> AggregateExport (toSym n) Nothing
    EThingWith _ (NoWildcard _) n ns -> AggregateExport (toSym n) (Just $ map toSym ns)
    EThingWith _ (EWildcard _ _) _ _ -> error "Not Supported: mixed export wildcards"

    EModuleContents _ n -> ModuleExport (toSym n)

-- | Convert from the third party export and extract the body
convertWithBody :: Spannable a => Text -> ExportSpec a -> WithBody Export
convertWithBody str export = WithBody (convert export) (ann export >< str)

-- | Parse an export
parse :: Text -> Either (SolutionError u) Export
parse s = case result of
    ParseOk ok
        | headAndImports <- unNonGreedy ok
        , ModuleHeadAndImports _ _ (Just modHead) _ <- headAndImports 
        , ModuleHead _ _ _ (Just specList) <- modHead
        , ExportSpecList _ exportList <- specList
        , [export] <- exportList
            -> Right $ convert export
      {-where 
        headAndImports = unNonGreedy ok
        ModuleHeadAndImports _ _ (Just modHead) _ = headAndImports
        ModuleHead _ _ _ (Just specList) = modHead
        ExportSpecList _ exportList = specList
        [export] = exportList-}
        | otherwise -> Left $ 
            ParseError (SrcFileLoc "" (toSrcLoc (Row 1, Column 1)))
                       "That isn't an export, and you know it" 
                       ""
    ParseFailed l msg -> Left $ ParseError (toSrcFileLoc l) msg ""
  where
    -- Parsing an export is done by putting in a mock export list, parsing that
    -- "module", and then pulling out the singleton list of exports
    dummyHeader = "module DUMMY (" <> s <> ") where"
    result :: (ParseResult (NonGreedy (ModuleHeadAndImports SrcSpanInfo)))
    result = Parser.parse (T.unpack dummyHeader)
