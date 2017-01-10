{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import PseudoState

import Ide3.Utils
import Ide3.Types
--import Ide3.NewMonad hiding (load, new, finalize)
--import qualified Ide3.NewMonad as M
import Ide3.NewMonad
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
    
    
    
instance (PersistenceClass m) => PersistenceClass (ViewerStateT m) where
    load = bounce load
    new = bounce . new
    finalize = bounce finalize

instance  (SolutionClass m) => SolutionClass (ViewerStateT m) where
    editSolutionInfo = bounce . editSolutionInfo
    addProject = bounce . addProject
    removeProject = bounce . removeProject
    getProjects = bounce getProjects
    editProjectInfo x = bounce . editProjectInfo x

instance  (ProjectModuleClass m) => ProjectModuleClass (ViewerStateT  m) where
    --addModule x = bounce . addModule x
    createModule x = bounce . createModule x
    removeModule x = bounce . removeModule x
    --getModule x = bounce . getModule x
    getModules = bounce . getModules
    --editModule x y = bounce . editModule x y
    getModuleHeader x = bounce . getModuleHeader x
    editModuleHeader x y = bounce . editModuleHeader x y
    setModuleUnparsable x y = bounce . setModuleUnparsable x y
    setModuleParsable x = bounce . setModuleParsable x
    getUnparsableModule x = bounce . getUnparsableModule x
    refreshModule x = bounce . refreshModule x

instance   (ProjectExternModuleClass m) => ProjectExternModuleClass (ViewerStateT  m) where
    --addExternModule x = bounce . addExternModule x
    createExternModule x = bounce . createExternModule x
    --getExternModule x = bounce . getExternModule x
    getExternModules = bounce . getExternModules
    removeExternModule x = bounce . removeExternModule x

instance   (ModuleDeclarationClass m) => ModuleDeclarationClass (ViewerStateT  m) where
    editDeclaration x y z = bounce . editDeclaration x y z
    addDeclaration x y = bounce . addDeclaration x y
    getDeclaration x y = bounce . getDeclaration x y
    getDeclarations x = bounce . getDeclarations x
    removeDeclaration x y = bounce . removeDeclaration x y

instance   (ModuleImportClass m) => ModuleImportClass (ViewerStateT  m) where
    addImport x y = bounce . addImport x y
    getImport x y = bounce . getImport x y
    removeImport x y = bounce . removeImport x y
    getImports x = bounce . getImports x

instance   (ModuleExportClass m) => ModuleExportClass (ViewerStateT  m) where
    addExport x y = bounce . addExport x y
    getExport x y = bounce . getExport x y
    removeExport x y = bounce . removeExport x y
    exportAll x = bounce . exportAll x
    exportNothing x = bounce . exportNothing x
    getExports x = bounce . getExports x

instance   (ModulePragmaClass m) => ModulePragmaClass (ViewerStateT  m) where
    addPragma x y = bounce . addPragma x y
    removePragma x y = bounce . removePragma x y
    getPragmas x = bounce . getPragmas x

instance (ExternModuleExportClass m) => ExternModuleExportClass (ViewerStateT m) where
    addExternExport x y = bounce . addExternExport x y
    getExternExport x y = bounce . getExternExport x y
    getExternExports x = bounce . getExternExports x
    removeExternExport x y = bounce . removeExternExport x y

instance (ModuleLocationClass m) => ModuleLocationClass (ViewerStateT m) where
    getModuleItemAtLocation = bounce .-... getModuleItemAtLocation

deriving instance (MonadMask m) => MonadMask (ViewerStateT m)
deriving instance (MonadCatch m) => MonadCatch (ViewerStateT m)
deriving instance (MonadThrow m) => MonadThrow (ViewerStateT m)
