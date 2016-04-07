module Ide3.Module.Parser where

import Language.Haskell.Exts.Annotated.Parser
import Language.Haskell.Exts.Parser (ParseResult(..), defaultParseMode)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Comments
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax

import Ide3.Types

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Export as Export
import qualified Ide3.Import as Import

data ExtractionResults
    = Extracted ModuleInfo 
                [WithBody Export] 
                [WithBody Import]
                [WithBody Declaration] 

extractInfo :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> ModuleInfo
extractInfo _ (Syntax.Module _ (Just (Syntax.ModuleHead _ (Syntax.ModuleName _ n) _ _)) _ _ _, _) = ModuleInfo (Symbol n)
extractInfo _ _ = UnamedModule Nothing

extractExports :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> [WithBody Export]
extractExports str (Syntax.Module _ (Just (Syntax.ModuleHead _ _ _ (Just (Syntax.ExportSpecList _ exports)))) _ _ _, _)
     = map (Export.convertWithBody str) exports
extractExports _ _ = []

extractImports :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> [WithBody Import]
extractImports str (Syntax.Module _ _ _ imports _, _)
    = map (Import.convertWithBody str) imports
extractImports _ _ = []

extractDecls :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> Either ProjectError [WithBody Declaration]
extractDecls str (Syntax.Module _ _ _ _ decls, _)
    = Declaration.combineMany <$> (sequence $ map (Declaration.convertWithBody str) decls)
extractDecls _ _ = Right []

extract :: String -> (Syntax.Module SrcSpanInfo, [Comment]) -> Either ProjectError ExtractionResults
extract str x = do
    let info    =  extractInfo      str x
        exports =  extractExports   str x
        imports =  extractImports   str x
    decls       <- extractDecls     str x
    return $ Extracted info exports imports decls

parse :: String -> Either ProjectError ExtractionResults
parse s = case parseModuleWithComments defaultParseMode s of
    ParseOk x -> extract s x
    ParseFailed _ msg -> Left msg

