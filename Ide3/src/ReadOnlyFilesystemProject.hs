{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : ReadOnlyFilesystemProject
Description : Read only persistence mechanism
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This modules provides a "persistance" mechanism which only permits the digesting
of existing standard haskell projects and then viewing/searching them.

Attempting to save/edit/create using this persistence mechanism will result in an error
-}
module ReadOnlyFilesystemProject
    ( ReadOnlyFilesystemProjectT( ReadOnlyFilesystemProjectT )
    , FileSystemProject (Unopened)
    , runReadOnlyFilesystemProjectT
    ) where

import System.FilePath

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.Mechanism.State
import Ide3.Types (SolutionError (..), ProjectInfo (..), SolutionInfo (..))
import Ide3.Digest

import PseudoState
import ViewerMonad

-- | State of the mechanism
data FileSystemProject
    -- | There is a path to be opened
    = ToOpen FilePath
    -- | No project opened
    | Unopened
    -- | A project at the path is opened
    | Opened SolutionInfo FilePath
    deriving Show

-- | State transformer for the mechanism
newtype ReadOnlyFilesystemProjectT m a
    = ReadOnlyFilesystemProjectT 
    { runReadOnlyFilesystemProjectTInternal 
        :: StateT FileSystemProject m a 
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , SolutionStateM
    )

--type ReadOnlyFilesystemProjectT' m = StatefulSolution (ReadOnlyFilesystemProjectT m)

-- | Run an action inside the mechanism with the provided state
runReadOnlyFilesystemProjectT :: MonadIO m => ReadOnlyFilesystemProjectT m a -> FileSystemProject -> m (a, FileSystemProject)
runReadOnlyFilesystemProjectT = runStateT . runReadOnlyFilesystemProjectTInternal

-- | Run an action inside the mechanism 
--runNewReadOnlyFilesystemProjectT :: MonadIO m => ReadOnlyFilesystemProjectT m a -> m (a, FileSystemProject)
--runNewReadOnlyFilesystemProjectT = flip runReadOnlyFilesystemProjectT Unopened

-- | Get the state of the project
getFsp :: (Monad m) => ReadOnlyFilesystemProjectT m FileSystemProject
getFsp = ReadOnlyFilesystemProjectT get

-- | Set the state of the project
putFsp :: (Monad m) => FileSystemProject -> ReadOnlyFilesystemProjectT m ()
putFsp = ReadOnlyFilesystemProjectT . put

{-
instance SolutionStateM m => SolutionStateM (ReadOnlyFilesystemProjectT m) where
    getProject = lift getProject
    putProject = lift . putProject
-}

instance MonadIO m => SolutionShellM (ReadOnlyFilesystemProjectT m) where
    -- | Not supported
    new _ = throwE $ Unsupported "Cannot create a new read-only project"
    -- | Digest a project after loading the interface file
    load = do
        fsp <- lift getFsp
        case fsp of
            ToOpen solutionPath -> do
                let parts = splitPath solutionPath
                    projectName = last parts
                    solutionName = last parts
                    project = ( ProjectInfo projectName
                              , solutionPath
                              , Just $ solutionPath </> "ifaces"
                              )
                p <- digestSolution (SolutionInfo solutionName) solutionPath [project]
                lift $ putFsp $ Opened (SolutionInfo solutionName) solutionPath
                return p
            Unopened -> throwE $ InvalidOperation "No path specified for opening" ""
            Opened info solutionPath -> do
                let (SolutionInfo solutionName) = info
                    projectName = solutionName
                    project = ( ProjectInfo projectName
                              , solutionPath
                              , Just $ solutionPath </> "ifaces"
                              )
                digestSolution info solutionPath [project]
    -- | Not supported
    finalize _ = throwE $ Unsupported "Cannot save a read-only project"


instance (MonadIO m, SolutionStateM m) => ViewerMonad (StatefulSolution (ReadOnlyFilesystemProjectT m)) where
    -- | Not supported
    setFileToOpen _ = throwE $ Unsupported "Cannot open a file in a readonly project"
    -- | Set the path to be digested
    setDirectoryToOpen x = lift $ lift $ putFsp $ ToOpen x
    -- | Unsupported
    setTargetPath _ = throwE $ Unsupported "Cannot set a target path for a readonly project"
    -- | Check if a project has been digested
    hasOpenedSolution = do
        fsp <- lift $ getFsp
        case fsp of
            Opened _ _ -> return True
            _ -> return False
    -- | Not supported
    createNewFile _ = throwE $ Unsupported "Cannot create a new readonly project"
    -- | Not supported
    createNewDirectory _ = throwE $ Unsupported "Cannot create a new readonly project"
    -- | Do nothing, as a read only project cannot make changes, so the project
    -- must allready be ready to build
    prepareBuild = return ()


instance PseudoStateT ReadOnlyFilesystemProjectT FileSystemProject where
    runPseudoStateT = runStateT . runReadOnlyFilesystemProjectTInternal
