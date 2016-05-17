{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : ReadOnlyFilesystemProject
Description : Read/Show persistence mechanism
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This modules provides a persistance mechanism which can digest existing projects
as ReadOnlyFilesystemProject can, but also can read and save projects from a
single file using the Read/Show instances for the Project type.

-}
module SimpleFilesystemProject where

import Data.List
import Data.Maybe

import Text.Read (readMaybe)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import System.Directory
import System.IO.Error

import System.FilePath

import Ide3.Monad hiding (load)
import Ide3.Mechanism.State
import Ide3.Types
import qualified Ide3.Project as Project
import Ide3.Digest
import Ide3.ModuleTree


import ViewerMonad



-- | State of the mechanism
data FileSystemProject
    -- | A file containing a dump of a Project is to be opened
    = ToOpen FilePath
    -- | A path is to be digested
    | ToDigest FilePath
    -- | No project opened
    | Unopened
    -- | A digested path or dump file is opened if Just, a new project if Nothing
    | Opened (Maybe FilePath)

-- | State transformer for the mechanism
type SimpleFilesystemProjectT = StateT FileSystemProject

-- | Run an action inside the mechanism with the provided state
runSimpleFilesystemProjectT :: MonadIO m => SimpleFilesystemProjectT m a -> FileSystemProject -> m (a, FileSystemProject)
runSimpleFilesystemProjectT = runStateT

-- | Run an action inside the mechanism 
runNewSimpleFilesystemProjectT :: MonadIO m => SimpleFilesystemProjectT m a -> m (a, FileSystemProject)
runNewSimpleFilesystemProjectT = flip runSimpleFilesystemProjectT Unopened

instance ProjectStateM m => ProjectStateM (SimpleFilesystemProjectT m) where
    getProject = lift getProject
    putProject = lift . putProject

instance MonadIO m => ProjectShellM (SimpleFilesystemProjectT m) where
    -- | Create a new project
    new i = do
        lift $ put $ Opened Nothing
        return $ Project.new i
    -- | Either digest a directory or open a file and use Read on the contents
    load = do
        fsp <- lift get
        case fsp of
            ToDigest path -> do
                p <- digestProject' path (Just "ifaces") 
                lift $ put $ Opened Nothing
                return p
            ToOpen path -> do
                result <- liftIO $ tryIOError $ readFile path
                case result of
                    Right contents -> case readMaybe contents of
                        Just p -> do
                            lift $ put $ Opened $ Just path
                            return p
                        Nothing -> throwE $ InvalidOperation "File did not contain a valid project" ""
                    Left err -> throwE $ InvalidOperation ("Error on opening file: " ++ show err) ""
            Unopened -> throwE $ InvalidOperation "No path specified for opening" ""
            Opened Nothing -> throwE $ InvalidOperation "Cannot re-open a digested project" ""
            Opened (Just path) -> do
                lift $ put $ ToOpen path
                load
    -- | Use Show to turn the current project into a string and write it to the
    -- correct file
    finalize p = do
        fsp <- lift get
        case fsp of
            Opened (Just path) -> do
                result <- liftIO $ tryIOError $ writeFile path $ show p
                case result of
                    Right _ -> return ()
                    Left err -> throwE $ InvalidOperation ("Error on writing file: " ++ show err) ""
            _ -> throwE $ InvalidOperation ("Cannot finalize a project without a path to write to") ""


moduleNamePath :: String -> FilePath
moduleNamePath [] = []
moduleNamePath ('/':xs) = '.' : moduleNamePath xs
moduleNamePath (x:xs) = x : moduleNamePath xs

modulePath :: ModuleInfo -> Maybe FilePath
modulePath (ModuleInfo (Symbol s)) = Just $ "src/" ++ moduleNamePath s ++ ".hs"
modulePath (UnamedModule path) = path

makeFileListing :: ProjectM m => ProjectResult m u ([FilePath],[(FilePath,String)])
makeFileListing = do
    t <- makeTree
    let dirs (OrgNode i ts) =  (: (concat $ mapMaybe dirs ts)) <$> (takeDirectory <$> modulePath i)
        dirs (ModuleNode i _ ts) = (: (concat $ mapMaybe dirs ts)) <$> (takeDirectory <$> modulePath i)
        declInfos (OrgNode _ ts) = concatMap declInfos ts
        declInfos (ModuleNode i ds ts) = (i, ds) : concatMap declInfos ts
        dirs' = concat $ mapMaybe dirs t
        declInfos' = concatMap declInfos t
    declGroups <- forM declInfos' $ \(mi, dis) -> do
        ds <- forM dis $ getDeclaration mi
        case modulePath mi of
            Nothing -> return Nothing 
            Just p -> return $ Just (p, intercalate "\n" $ map body ds)
    let decls = catMaybes declGroups
    return (dirs', decls)

instance (MonadIO m, ProjectStateM m) => ViewerMonad (SimpleFilesystemProjectT m) where
    -- | Set the Read file to be opened
    setFileToOpen path = lift $ put $ ToOpen path
    -- | Set the path to be digested
    setDirectoryToOpen path = lift $ put $ ToDigest path
    -- | Set the path to Show to
    setTargetPath path = do
        fsp <- lift get
        case fsp of
            Opened _ -> lift $ put $ Opened (Just path)
            _ -> throwE $ InvalidOperation "Cannot set target path without open project" ""
    -- | Check if either there is a new project, digested path, or Read'd file
    hasOpenedProject = do
        fsp <- get
        case fsp of
            Opened _ -> return True
            _ -> return False
    createNewFile path = do
        lift $ lift $ putProject $ Project.new ProjectInfo
        lift $ put $ Opened (Nothing)
        setTargetPath path
    createNewDirectory _ = throwE $ Unsupported "Cannot create a directory project using simple"
    prepareBuild = do
        (dirs,files) <- makeFileListing
        liftIO $ forM_ dirs $ createDirectoryIfMissing True
        liftIO $ forM_ files $ \(path,contents) -> writeFile path contents
        
