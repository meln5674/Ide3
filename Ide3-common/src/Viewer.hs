{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : Viewer
Description : Viewer for the demo solution
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module defines a monad transformer, ViewerStateT, which adds access to the
state of the program, and uses the ViewerMonad to wrap around various
funcitonality.
-}
module Viewer 
    ( module Viewer
    , module ViewerMonad
    ) where

import Data.Maybe

import System.Directory

import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

import PseudoState

import Ide3.Utils
import Ide3.Types
--import Ide3.NewMonad hiding (load, new, finalize)
--import qualified Ide3.NewMonad as M
import Ide3.NewMonad
import Ide3.NewMonad.Instances.Lift.TH
import Ide3.Types (SolutionError (..), DeclarationInfo(..), ModuleInfo(..))

import ViewerMonad

class ViewerMonad m => ViewerStateClass m where
    getCurrentProject :: m (Maybe ProjectInfo)
    getCurrentModule :: m (Maybe (ProjectInfo, ModuleInfo))
    getCurrentDeclaration :: m (Maybe (ProjectInfo, ModuleInfo,DeclarationInfo))
    setCurrentProject :: ProjectInfo -> m ()
    setCurrentModule :: ProjectInfo -> ModuleInfo -> m ()
    -- | Set the current declaration of the program
    setCurrentDecl :: ProjectInfo -> ModuleInfo -> DeclarationInfo -> m ()
    setNoCurrentDecl :: m ()

-- | The state of the program
data ViewerState
    = Viewer 
    { currentProject :: Maybe ProjectInfo 
    , currentModule :: Maybe ModuleInfo
    , currentDecl :: Maybe DeclarationInfo 
    }

-- | Transformer which adds access to the state of the program
newtype ViewerStateT m a = ViewerStateT { runViewerStateTInternal :: StateT ViewerState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, ViewerMonad)

  
instance MonadBounce ViewerStateT where
    bounce = ExceptT . ViewerStateT . lift . runExceptT

instance PseudoStateT ViewerStateT ViewerState where
    runPseudoStateT = runStateT . runViewerStateTInternal

emptyViewer :: ViewerState
emptyViewer = Viewer Nothing Nothing Nothing

runViewerStateT :: Monad m => ViewerStateT m a -> ViewerState -> m a
runViewerStateT f = evalStateT (runViewerStateTInternal f)

{-
-- | The ViewerStateT transformer applied to another transformer, applied to
-- the solution state transformer applied to IO
type ViewerStateM fsp t = ViewerStateT (t (SolutionStateT IO))

-- | A token which can be used to resume the program
data ViewerResume fsp = Resume ViewerState fsp Solution
-}

{-
instance SolutionStateM m => SolutionStateM (StateT s m) where
    getSolution = lift getSolution
    putSolution = lift . putSolution
-}

{-
-- | Run the viewer state transformer with a given state
runViewerStateT :: Monad m => ViewerStateT m a -> ViewerState -> m (a,ViewerState)
runViewerStateT = runStateT

-- | Run the viewer state transformer with the initial program state
runNewViewerStateT :: Monad m => ViewerStateT m a -> m (a,ViewerState)
runNewViewerStateT = flip runViewerStateT $ Viewer Nothing Nothing Nothing

runViewerState :: (MonadIO (t (SolutionStateT IO)))
               => (forall b . t (SolutionStateT IO) b -> fsp -> SolutionStateT IO (b, fsp))
               -> fsp 
               -> ViewerStateM fsp t a 
               -> IO (a,ViewerResume fsp)
runViewerState runFSPT unopened f = resumeViewerState 
    f 
    runFSPT
    (Resume (Viewer Nothing Nothing Nothing) unopened initialSolution)

-- | Resume the viewer state transformer
resumeViewerState :: 
                     (MonadIO (t (SolutionStateT IO)))
                  => ViewerStateM fsp t a 
                  -> (forall b . t (SolutionStateT IO) b -> fsp -> SolutionStateT IO (b,fsp))
                  -> ViewerResume fsp 
                  -> IO (a,ViewerResume fsp)
resumeViewerState f runFSPT (Resume viewer fsp proj) = do
    let runViewer = runViewerStateT f viewer
        runFSP = runFSPT runViewer fsp
        runSolution = runSolutionStateT runFSP proj
    (((result,viewer'),fsp'),proj') <- runSolution
    return (result,Resume viewer' fsp' proj')
-}

hasCurrentProject :: (ViewerStateClass m) => m Bool
hasCurrentProject = liftM isJust getCurrentProject

-- | Check if the program currently has a module open
hasCurrentModule :: (ViewerStateClass m) => m Bool
hasCurrentModule = liftM isJust getCurrentModule


hasCurrentDeclaration :: (ViewerStateClass m) => m Bool
hasCurrentDeclaration = liftM isJust getCurrentDeclaration

-- | Open a solution at a given path
openSolution :: ( MonadIO m, ViewerStateClass m, PersistenceClass m )
            => FilePath 
            -> SolutionResult u m ()
openSolution path = do
    isFile <- liftIO $ doesFileExist path
    isDir <- liftIO $ doesDirectoryExist path
    case (isFile, isDir) of
        (True, _) -> do
            setFileToOpen path
            lift $ setNoCurrentDecl
            load
        (_,True) -> do
            setDirectoryToOpen path
            lift $ setNoCurrentDecl
            load
        (_,_) -> throwE $ InvalidOperation (path ++ " does not exist") ""

-- | Save the current solution, optionally with a new path to save to
saveSolution :: (ViewerMonad m, PersistenceClass m, ViewerStateClass m) 
            => Maybe FilePath
            -> SolutionResult u m ()
saveSolution maybePath = do
    cond <- lift hasOpenedSolution
    if cond
        then do 
                case maybePath of
                    Just path -> setTargetPath path
                    Nothing -> return ()
                finalize
        else throwE $ InvalidOperation "No solution is currently open" ""


instance ViewerMonad m => ViewerStateClass (ViewerStateT m) where
    setCurrentProject pji = ViewerStateT $ put $ Viewer (Just pji) Nothing Nothing
    setCurrentModule pji mi = ViewerStateT $ put $ Viewer (Just pji) (Just mi) Nothing
    setCurrentDecl pji mi di = ViewerStateT $ put $ Viewer (Just pji) (Just mi) (Just di)
    getCurrentProject = ViewerStateT $ gets currentProject
    getCurrentModule = ViewerStateT $ do
        maybePji <- gets currentProject
        maybeMi <- gets currentModule
        case (maybePji, maybeMi) of
            (Just pji, Just mi) -> return $ Just (pji,mi)
            _ -> return Nothing
    getCurrentDeclaration = ViewerStateT $ do
        maybePji <- gets currentProject
        maybeMi <- gets currentModule
        maybeDi <- gets currentDecl
        case (maybePji, maybeMi, maybeDi) of
            (Just pji, Just mi, Just di) -> return $ Just (pji,mi,di)
            _ -> return Nothing
    setNoCurrentDecl = ViewerStateT $ put $ Viewer Nothing Nothing Nothing
    

[betterderiving|
splice2;
;
  PersistenceClass
, SolutionClass
, ProjectModuleClass
, ModuleDeclarationClass
, ModuleImportClass
, ModuleExportClass
, ModulePragmaClass
, ModuleFileClass
, ModuleLocationClass
, ProjectExternModuleClass
, ExternModuleExportClass;
ViewerStateT;
|]

deriving instance (MonadMask m) => MonadMask (ViewerStateT m)
deriving instance (MonadCatch m) => MonadCatch (ViewerStateT m)
deriving instance (MonadThrow m) => MonadThrow (ViewerStateT m)
