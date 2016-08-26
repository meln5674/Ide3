{-|
Module      : Ide3.Mechanism.Internal
Description : Convienience functions for the Project monad
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

TODO: Description
-}
module Ide3.Mechanism.Internal where

import Control.Monad
import Control.Monad.Trans.Except

import Ide3.Types 
import Ide3.Module.Common (EitherModule)
import qualified Ide3.Module as Module 
import qualified Ide3.Module.Query as Module 
import qualified Ide3.Module.Extern as ExternModule 
import qualified Ide3.Import as Import 
import qualified Ide3.Export as Export 
import qualified Ide3.Declaration as Declaration

import Ide3.Monad

{-
f :: Monad m => Either e a -> (a -> ExceptT e m a) -> ExceptT e m a
f p s = case p of
    Right x -> s x
    Left e -> throwE e
-}

-- | Parse an import and add it to a module
addRawImport :: SolutionM m => ProjectInfo -> ModuleInfo -> String -> SolutionResult u m ImportId
addRawImport pji mi str = case Import.parse str of
    Right i -> addImport pji mi $ WithBody i str
    Left err -> throwE err 

-- | Parse an export and add it to a module
addRawExport :: SolutionM m => ProjectInfo -> ModuleInfo -> String -> SolutionResult u m ExportId
addRawExport pji mi str = case Export.parse str of
    Right e -> addExport pji mi $ WithBody e str
    Left err -> throwE err

-- | Parse a declaration and add it to a module
addRawDeclaration :: SolutionM m => ProjectInfo -> ModuleInfo -> String -> SolutionResult u m ()
addRawDeclaration pji mi str = case Declaration.parse str of
    Right d -> addDeclaration pji mi $ WithBody d str
    Left err -> throwE err

-- | Parse an entire module and add it to the project
addRawModule :: SolutionM m => ProjectInfo -> String -> Maybe FilePath -> SolutionResult u m ModuleInfo
addRawModule pji str p = case Module.parse str p of
    Right (m,_,_) -> do
        addModule pji m
        return $ Module.info m
    Left err -> throwE err

-- | Get either an internal or external module
getAnyModule :: SolutionM m => ProjectInfo -> ModuleInfo -> SolutionResult u m EitherModule
getAnyModule pji mi = catchE tryLocal $ const tryExtern
  where
    tryLocal = liftM Left $ getModule pji mi
    tryExtern = liftM Right $ getExternModule pji mi
    

-- | Get the symbols exported by a module
getExternalSymbols :: SolutionM m => ProjectInfo -> ModuleInfo -> SolutionResult u m  [Symbol]
getExternalSymbols pji mi = do
    m <- getAnyModule pji mi
    case m of
        Left lm -> liftM (map getChild) $ Module.exportedSymbols pji lm
        Right em -> return $ map getChild $ ExternModule.exportedSymbols em

-- | Get the symbols availible at the top level of a module
getInternalSymbols :: SolutionM m 
                   => ProjectInfo 
                   -> ModuleInfo 
                   -> SolutionResult u m  [Symbol]
getInternalSymbols pji mi = do
    m <- getModule pji mi 
    Module.internalSymbols pji m

{-

renameModule :: SolutionM m => ModuleInfo -> ModuleInfo -> SolutionResult u m ()
renameModule (ModuleInfo src) (ModuleInfo dest) = do
    editModule src $ \(Module _ ps es is ds) -> Right $ Module dest ps es is ds
    allModules <- getModules
    forM_ allModules $ \mi -> do
        allImports <- getImports mi
        forM_ allImports $ \ii -> do
            i <- getImport mi ii
            when (Import.moduleName i == src) $ do
                let i' = Import.editModuleName (const dest) i
                removeImport mi ii
                addImport mi i'
renameModule _ _ = throwE $ InvalidOperation "Cannot rename from or to an unnamed module"
-}
