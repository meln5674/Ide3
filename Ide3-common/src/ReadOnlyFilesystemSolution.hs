{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ReadOnlyFilesystemSolution
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
{-# LANGUAGE TypeFamilies #-}
module ReadOnlyFilesystemSolution
    ( ReadOnlyFilesystemSolutionT( ReadOnlyFilesystemSolutionT )
    , FileSystemSolution (Unopened)
    , runReadOnlyFilesystemSolutionT
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import System.FilePath

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.NewMonad.Instances.State.Class
import Ide3.Types (SolutionError (..), ProjectInfo (..), SolutionInfo (..))
import Ide3.Digest

import PseudoState
import ViewerMonad

-- | State of the mechanism
data FileSystemSolution
    -- | No project opened
    = Unopened
    -- | A project at the path is opened
    | Opened SolutionInfo FilePath
    deriving Show

-- | State transformer for the mechanism
newtype ReadOnlyFilesystemSolutionT m a
    = ReadOnlyFilesystemSolutionT 
    { runReadOnlyFilesystemSolutionTInternal 
        :: StateT FileSystemSolution m a 
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    )

type PersistToken = FilePath

--type ReadOnlyFilesystemSolutionT' m = StatefulSolution (ReadOnlyFilesystemSolutionT m)

-- | Run an action inside the mechanism with the provided state
runReadOnlyFilesystemSolutionT :: ReadOnlyFilesystemSolutionT m a -> FileSystemSolution -> m (a, FileSystemSolution)
runReadOnlyFilesystemSolutionT = runStateT . runReadOnlyFilesystemSolutionTInternal

-- | Run an action inside the mechanism 
--runNewReadOnlyFilesystemSolutionT :: MonadIO m => ReadOnlyFilesystemSolutionT m a -> m (a, FileSystemSolution)
--runNewReadOnlyFilesystemSolutionT = flip runReadOnlyFilesystemSolutionT Unopened

-- | Get the state of the project
getFsp :: (Monad m) => ReadOnlyFilesystemSolutionT m FileSystemSolution
getFsp = ReadOnlyFilesystemSolutionT get

-- | Set the state of the project
putFsp :: (Monad m) => FileSystemSolution -> ReadOnlyFilesystemSolutionT m ()
putFsp = ReadOnlyFilesystemSolutionT . put

{-
instance SolutionStateM m => SolutionStateM (ReadOnlyFilesystemSolutionT m) where
    getSolution = lift getSolution
    putSolution = lift . putSolution
-}

instance MonadIO m => StatefulPersistenceClass (ReadOnlyFilesystemSolutionT m) where
    type StatefulPersistToken (ReadOnlyFilesystemSolutionT m) = PersistToken
    -- | Not supported
    newState _ = throwE $ Unsupported "Cannot create a new read-only project"
    -- | Digest a project after loading the interface file
    loadState solutionPath = do
        let parts = splitPath solutionPath
            projectName = T.pack $ last parts
            solutionName = T.pack $ last parts
            project = Params
                        (ProjectInfo projectName)
                        solutionPath
                        (Just $ solutionPath </> "ifaces")
        p <- digestSolution (SolutionInfo solutionName) [project]
        lift $ putFsp $ Opened (SolutionInfo solutionName) solutionPath
        return p
    -- | Not supported
    finalizeState _ _ = throwE $ Unsupported "Cannot save a read-only project"


instance (MonadIO m) => ViewerMonad (ReadOnlyFilesystemSolutionT m) where
    type ViewerPersistToken (ReadOnlyFilesystemSolutionT m) = PersistToken
    preparePathToken = return
{-
    -- | Not supported
    setFileToOpen _ = throwE $ Unsupported "Cannot open a file in a readonly project"
    -- | Set the path to be digested
    setDirectoryToOpen x = lift $ putFsp $ ToOpen x
    -- | Unsupported
    setTargetPath _ = throwE $ Unsupported "Cannot set a target path for a readonly project"
-}
    -- | Check if a project has been digested
    hasOpenedSolution = do
        fsp <- getFsp
        case fsp of
            Opened _ _ -> return True
            _ -> return False
{-
    -- | Not supported
    createNewFile _ = throwE $ Unsupported "Cannot create a new readonly project"
    -- | Not supported
    createNewDirectory _ = throwE $ Unsupported "Cannot create a new readonly project"
-}
    -- | Do nothing, as a read only project cannot make changes, so the project
    -- must allready be ready to build
    prepareBuild = return ()


instance PseudoStateT ReadOnlyFilesystemSolutionT FileSystemSolution where
    runPseudoStateT = runStateT . runReadOnlyFilesystemSolutionTInternal

