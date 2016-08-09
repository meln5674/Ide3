module Ide3.Refactor where

import Control.Monad
import Control.Monad.Trans.Except

import Ide3.Types.Internal

import qualified Ide3.Import as Import
import qualified Ide3.Module as Module
import qualified Ide3.Declaration as Declaration

import Ide3.NewMonad
import Ide3.Utils


-- | Take a declaration and rename a list of symbols
renameSymbols :: (SolutionMonad m)
             => ProjectInfo
             -> ModuleInfo
             -> DeclarationInfo
             -> [(Symbol,Symbol)]
             -> SolutionResult m u DeclarationInfo
renameSymbols pji mi di pairs = do
    let edit str = foldr (\(Symbol old,Symbol new) str -> replace old new str) str pairs
    editDeclaration pji mi di $ \d -> do
        let str = body d
            str' = edit str
            parseResult = Declaration.parseAndCombineLenient str' Nothing $ Declaration.info $ item d
        return $ case parseResult of
            Left (d',err) -> d'
            Right d' -> d'
                    

-- | Rename a module in a project, and find any place where that module's name
-- is part of a symbol in a declaration and change it
renameModule :: (SolutionMonad m)
             => ProjectInfo
             -> ModuleInfo
             -> ModuleInfo
             -> SolutionResult m u ()
renameModule pji src@(ModuleInfo msym) dest@(ModuleInfo msym') = do
    projectResult <- Module.importedBy pji src
    forM_ projectResult $ \(ProjectChild pji' moduleResult) -> do
        forM_ moduleResult $ \(ModuleChild mi' iis) -> do
            forM_ iis $ \ii -> do
                i <- getImport pji' mi' ii
                syms <- Import.symbolsProvided' pji' $ item i
                let i' = Import.editModuleName (const msym') i
                syms' <- Import.symbolsProvided' pji' $ item i'
                let pairs = filter (uncurry (/=)) $ zip syms syms'
                dis <- getDeclarations pji' mi'
                forM dis $ \di -> renameSymbols pji' mi' di pairs
