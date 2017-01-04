{-|
Module      : Ide3.Refactor
Description : Refactoring solutions
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}
module Ide3.Refactor where

import Control.Monad
import Control.Monad.Trans.Except

import Ide3.Types.Internal

import qualified Ide3.Import as Import
import qualified Ide3.Module as Module
import qualified Ide3.Declaration as Declaration

import Ide3.NewMonad
import Ide3.Utils


-- | Rename a list of symbols in a declaration
renameSymbols :: (SolutionMonad m)
             => ProjectInfo
             -> ModuleInfo
             -> DeclarationInfo
             -> [(Symbol,Symbol)]
             -> SolutionResult u m DeclarationInfo
renameSymbols pji mi di pairs = do
    let edit str = foldr replace' str pairs
        replace' (Symbol src, Symbol dest) str' = replace src dest str'
    editDeclaration pji mi di $ \d -> do
        let str = body d
            str' = edit str
            declInfo = Declaration.info $ item d
            parseResult =
                Declaration.parseAndCombineLenient str' Nothing declInfo
        return $ case parseResult of
            Left (d',_) -> d'
            Right d' -> d'
                    

-- | Rename a module in a project, and find any place where that module's name
-- is part of a symbol in a declaration and change it
renameModule :: (SolutionMonad m)
             => ProjectInfo
             -> ModuleInfo
             -> ModuleInfo
             -> SolutionResult u m ()
renameModule pji src (ModuleInfo msym') = do
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
renameModule _ _ _ =
    throwE $ InvalidOperation "Cannot rename a module to unnamed" ""
