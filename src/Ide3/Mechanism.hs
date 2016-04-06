{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Mechanism where

import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.State

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
    Left msg -> return $ Left $ "Failed to parse import" ++ msg
addRawDeclaration :: ProjectM m => ModuleInfo -> String -> m (Either ProjectError ())
addRawDeclaration i s = case Declaration.parse s of
    Right d -> do addDeclaration i (WithBody d s)
                  return $ Right ()
    Left msg -> return $ Left $ "Failed to parse declaration" ++ msg

getExternalSymbols :: ProjectM m => ModuleInfo -> m (Either ProjectError [Symbol])
getExternalSymbols m = runExceptT $ ExceptT (getModule m) >>= Module.exportedSymbols >>= return . map getChild

getInternalSymbols :: ProjectM m => ModuleInfo -> m (Either ProjectError [Symbol])
getInternalSymbols m = runExceptT $ ExceptT (getModule m) >>= Module.internalSymbols

type ProjectState = State Project

runProjectState :: ProjectState a -> (a,Project)
runProjectState f = runState f (Project.new ProjectInfo)

modifyEither :: MonadState s m => (s -> Either l s) -> m (Either l ())
modifyEither f = modifyEitherR (\s -> (\r -> (r,())) <$> f s)

modifyEitherR :: MonadState s m => (s -> Either l (s,a)) -> m (Either l a)
modifyEitherR f = do
    s <- get
    let r = f s
    case r of
        Right (s',x) -> do put s'
                           return $ Right x
        Left l -> return $ Left l

instance ProjectM ProjectState where
    load = error "Cannot load a test project"
    new i = put $ Project.new i
    finalize = error "Cannot finalize a test project"
    editProjectInfo f = modify $ \(Project i ms b) -> (Project (f i) ms b)
    addModule m = modifyEither $ \p -> Project.addModule p m
    createModule i = modifyEither $ \p -> Project.createModule p i
    getModule i = gets $ \p -> Project.getModule p i
    removeModule i = modifyEither $ \p -> Project.removeModule p i
    addDeclaration i d = modifyEither $ \p -> Project.addDeclaration p i d
--        case Project.addDeclaration p i d of
--            Right p' -> Right p'
--            Left msg -> Left $ "Failed to add declaration: " ++ msg
    addImport mi i = modifyEitherR $ \p -> Project.addImport p mi i
--        case Project.addImport p mi i of
--            Right p' -> Right p'
--            Left msg -> Left $ "Failed to add import: " ++ msg
    removeImport mi i = modifyEither $ \p -> Project.removeImport p mi i
--        case Project.removeImport p mi i of
--            Right p' -> Right p'
--            Left msg -> Left $ "Failed to remove import: " ++ msg
    addExport mi e = modifyEitherR $ \p -> Project.addExport p mi e
--        case Project.addExport p mi e of
--            Right p' -> Right p'
--            Left msg -> error $ "Failed to add export: " ++ msg
    removeExport mi e = modifyEither $ \p -> Project.removeExport p mi e
--        case Project.removeExport p mi e of
--            Right p' -> p'
--            Left msg -> error $ "Failed to remove export" ++ msg
    exportAll mi = modifyEither $ \p -> Project.exportAll p mi
--        case Project.exportAll p mi of
--            Right p' -> p'
--            Left msg -> error $ "Failed to export all: " ++ msg
    

