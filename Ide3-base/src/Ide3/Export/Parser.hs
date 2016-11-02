{-|
Module      : Ide3.Export.Parser
Description : Parsing exports
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

module Ide3.Export.Parser (parse, convertWithBody) where

import Language.Haskell.Exts.Parser (ParseResult(..))
import Language.Haskell.Exts.Annotated.Parser hiding (parse)
import qualified Language.Haskell.Exts.Annotated.Parser as Parser
import Language.Haskell.Exts.Annotated.Syntax hiding (Module)
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
    EThingAll _ n -> AggregateExport (toSym n) Nothing
    EThingWith _ n ns -> AggregateExport (toSym n) (Just $ map toSym ns)
    EModuleContents _ n -> ModuleExport (toSym n)

-- | Convert from the third party export and extract the body
convertWithBody :: Spannable a => String -> ExportSpec a -> WithBody Export
convertWithBody str export = WithBody (convert export) (ann export >< str)

-- | Parse an export
parse :: String -> Either (SolutionError u) Export
parse s = case result of
    ParseOk ok -> Right $ convert export
      where 
        headAndImports = unNonGreedy ok
        ModuleHeadAndImports _ _ (Just head) _ = headAndImports
        ModuleHead _ _ _ (Just specList) = head
        ExportSpecList _ exportList = specList
        [export] = exportList
    ParseFailed l msg -> Left $ ParseError (toSrcFileLoc l) msg ""
  where
    -- Parsing an export is done by putting in a mock export list, parsing that
    -- "module", and then pulling out the singleton list of exports
    dummyHeader = "module DUMMY (" ++ s ++ ") where"
    result :: (ParseResult (NonGreedy (ModuleHeadAndImports SrcSpanInfo)))
    result = Parser.parse dummyHeader
