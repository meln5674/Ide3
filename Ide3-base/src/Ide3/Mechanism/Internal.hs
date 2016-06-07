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
addRawImport :: ProjectM m => ModuleInfo -> String -> ProjectResult m u ImportId
addRawImport mi s = case Import.parse s of
    Right i -> addImport mi (WithBody i s)
    Left err -> throwE err 

-- | Parse an export and add it to a module
addRawExport :: ProjectM m => ModuleInfo -> String -> ProjectResult m u  ExportId
addRawExport mi s = case Export.parse s of
    Right e -> addExport mi (WithBody e s)
    Left err -> throwE err

-- | Parse a declaration and add it to a module
addRawDeclaration :: ProjectM m => ModuleInfo -> String -> ProjectResult m u ()
addRawDeclaration i s = case Declaration.parse s of
    Right d -> void $ addDeclaration i (WithBody d s)
    Left err -> throwE err
    
-- | Parse an entire module and add it to the project
addRawModule :: ProjectM m => String -> Maybe FilePath -> ProjectResult m u  ModuleInfo
addRawModule s p = case Module.parse s p of
    Right (m,_,_) -> do
        addModule m
        return $ Module.info m
    Left err -> throwE err

-- | Get either an internal or external module
getAnyModule :: ProjectM m => ModuleInfo -> ProjectResult m u EitherModule
getAnyModule i = catchE (liftM Left $ getModule i) $ \_ -> liftM Right $ getExternModule i

-- | Get the symbols exported by a module
getExternalSymbols :: ProjectM m => ModuleInfo -> ProjectResult m u  [Symbol]
getExternalSymbols i = do
    m <- getAnyModule i
    case m of
        Left lm -> liftM (map getChild) $ Module.exportedSymbols lm
        Right em -> return $ map getChild $ ExternModule.exportedSymbols em

-- | Get the symbols availible at the top level of a module
getInternalSymbols :: ProjectM m => ModuleInfo -> ProjectResult m u  [Symbol]
getInternalSymbols m = getModule m >>= Module.internalSymbols

{-

renameModule :: ProjectM m => ModuleInfo -> ModuleInfo -> ProjectResult m u ()
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
