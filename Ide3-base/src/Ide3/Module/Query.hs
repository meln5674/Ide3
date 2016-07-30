module Ide3.Module.Query where

import qualified Data.Map as Map

import Control.Monad.Trans.Except

import Ide3.Types
import Ide3.Monad
import Ide3.Module

import qualified Ide3.Import as Import
import qualified Ide3.Export as Export
import qualified Ide3.Declaration as Declaration

-- | Test if a module imports another
importsModule :: Module -> Symbol -> Bool
importsModule m sym = sym `elem` map Import.moduleName (items $ Map.elems $ moduleImports m)

-- | Within the context of a project, find all of the symbols this module exports
--  This requires the project context as modules may export other modules,
--      necessitating finding what symbols they export, and so on
exportedSymbols :: SolutionM m => ProjectInfo -> Module -> SolutionResult m u [ModuleChild Symbol]
exportedSymbols pji m = case m of
    (Module _ _ _ Nothing _) -> return $ allSymbols m
    (Module _ _ _ (Just es) _) -> do
        syms <- concat <$> mapM (Export.symbolsProvided pji m . item) es
        return $ map (qualify m) syms

-- | Within the context of a project, find all of the symbosl being imported by
-- a module
importedSymbols :: SolutionM m 
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [Symbol]
importedSymbols pji m = concat <$> mapM providedBy imports
  where
    imports = moduleImports m
    providedBy = Import.symbolsProvided pji . item


-- | Within the context of a project, find all of the symbols which are visible
--  at the top level of this module 
internalSymbols :: SolutionM m 
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [Symbol]
internalSymbols pji m = do
    let decls = moduleDeclarations m
    importSyms <- importedSymbols pji m
    return $ importSyms ++ concatMap (Declaration.symbolsProvided . item) decls


-- | Given a sub-symbol, (such as a data constructor or a class method), find
--  the parent symbol and its siblings
--  If successful, the list will contain the parent symbol as its head, and the
--      siblings as the tail. The symbol provided will not be an item in the list
--  If the symbol is imported, it will be tagged as such
symbolTree :: SolutionM m 
           => ProjectInfo
           -> Module 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
symbolTree pji m sym = do
    let declarations = items $ Map.elems $ moduleDeclarations m
        declSearchResult = search (`Declaration.otherSymbols` sym) declarations
    case declSearchResult of
        Just (_,syms) -> return $ map (qualify m) syms
        Nothing -> do
            let imports = items $ Map.elems $ moduleImports m
                othersFromImport x = Import.otherSymbols pji x sym
            importSearchResult <- searchM othersFromImport imports
            case importSearchResult of
                Just (i,_) -> do
                    otherSyms <- Import.symbolTree pji i sym
                    return $ map (qualify m) otherSyms
                Nothing -> throwE $ SymbolNotFound (info m) sym "Module.symbolTree"
