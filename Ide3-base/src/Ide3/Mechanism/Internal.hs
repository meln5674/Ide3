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
addRawImport :: SolutionM m => ProjectInfo -> ModuleInfo -> String -> SolutionResult m u ImportId
addRawImport pi mi str = case Import.parse str of
    Right i -> addImport pi mi $ WithBody i str
    Left err -> throwE err 

-- | Parse an export and add it to a module
addRawExport :: SolutionM m => ProjectInfo -> ModuleInfo -> String -> SolutionResult m u ExportId
addRawExport pi mi str = case Export.parse str of
    Right e -> addExport pi mi $ WithBody e str
    Left err -> throwE err

-- | Parse a declaration and add it to a module
addRawDeclaration :: SolutionM m => ProjectInfo -> ModuleInfo -> String -> SolutionResult m u ()
addRawDeclaration pi mi str = case Declaration.parse str of
    Right d -> addDeclaration pi mi $ WithBody d str
    Left err -> throwE err

-- | Parse an entire module and add it to the project
addRawModule :: SolutionM m => ProjectInfo -> String -> Maybe FilePath -> SolutionResult m u ModuleInfo
addRawModule pi str p = case Module.parse str p of
    Right (m,_,_) -> do
        addModule pi m
        return $ Module.info m
    Left err -> throwE err

-- | Get either an internal or external module
getAnyModule :: SolutionM m => ProjectInfo -> ModuleInfo -> SolutionResult m u EitherModule
getAnyModule pi mi = catchE (liftM Left $ getModule pi mi) $ \_ -> liftM Right $ getExternModule pi mi

-- | Get the symbols exported by a module
getExternalSymbols :: SolutionM m => ProjectInfo -> ModuleInfo -> SolutionResult m u  [Symbol]
getExternalSymbols pi mi = do
    m <- getAnyModule pi mi
    case m of
        Left lm -> liftM (map getChild) $ Module.exportedSymbols pi lm
        Right em -> return $ map getChild $ ExternModule.exportedSymbols em

-- | Get the symbols availible at the top level of a module
getInternalSymbols :: SolutionM m 
                   => ProjectInfo 
                   -> ModuleInfo 
                   -> SolutionResult m u  [Symbol]
getInternalSymbols pi mi
    = getModule pi mi 
    >>= Module.internalSymbols pi

{-

renameModule :: SolutionM m => ModuleInfo -> ModuleInfo -> SolutionResult m u ()
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
