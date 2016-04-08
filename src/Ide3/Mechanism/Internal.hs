{-|
Module      : Ide3.Mechanism.Internal
Description : TODO
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Mechanism.Internal where

import Control.Monad.Trans.Except

import Ide3.Types 
import qualified Ide3.Project as Project 
import qualified Ide3.Module as Module 
import qualified Ide3.Import as Import 
import qualified Ide3.Export as Export 
import qualified Ide3.Declaration as Declaration

import Ide3.Monad

addRawImport :: ProjectM m => ModuleInfo -> String -> m (Either ProjectError ImportId)
addRawImport mi s = case Import.parse s of
    Right i -> addImport mi (WithBody i s)
    Left msg -> return $ Left $ "Failed to parse import: " ++ msg
addRawExport :: ProjectM m => ModuleInfo -> String -> m (Either ProjectError ExportId)
addRawExport mi s = case Export.parse s of
    Right e -> addExport mi (WithBody e s)
    Left msg -> return $ Left $ "Failed to parse export: " ++ msg
addRawDeclaration :: ProjectM m => ModuleInfo -> String -> m (Either ProjectError ())
addRawDeclaration i s = case Declaration.parse s of
    Right d -> do addDeclaration i (WithBody d s)
                  return $ Right ()
    Left msg -> return $ Left $ "Failed to parse declaration: " ++ msg

addRawModule :: ProjectM m => String -> m (Either ProjectError ModuleInfo)
addRawModule s = case Module.parse s of
    Right (m,eids,iids) -> do addModule m
                              return $ Right $ Module.info m
    Left msg -> return $ Left $ "Failed to parse module: " ++ msg

getExternalSymbols :: ProjectM m => ModuleInfo -> m (Either ProjectError [Symbol])
getExternalSymbols m = runExceptT $ ExceptT (getModule m) >>= Module.exportedSymbols >>= return . map getChild

getInternalSymbols :: ProjectM m => ModuleInfo -> m (Either ProjectError [Symbol])
getInternalSymbols m = runExceptT $ ExceptT (getModule m) >>= Module.internalSymbols
