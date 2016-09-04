{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module GuiViewer where

import Data.Text (Text)

import Data.List (delete)

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.Utils
import Ide3.NewMonad
import Ide3.Types

import Viewer

import PseudoState

import ViewerMonad

import SearchMode 

import History (History)
import qualified History

import GuiViewer.Class

import DeclarationPath

data GuiViewerState
    = GuiViewerState
    { openDeclarations :: [SolutionPath]
    , searchMode :: SearchMode
    , declarationHistory :: History (SolutionPath, Text)
    }

emptyGuiViewer :: GuiViewerState
emptyGuiViewer = GuiViewerState [] Find History.empty

newtype GuiViewerT m a = GuiViewer { runGuiViewer :: StateT GuiViewerState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, ViewerMonad)

deriving instance (MonadMask m) => MonadMask (GuiViewerT m)
deriving instance (MonadCatch m) => MonadCatch (GuiViewerT m)
deriving instance (MonadThrow m) => MonadThrow (GuiViewerT m)

instance Monad m => GuiViewerClass (GuiViewerT m) where
    setSearchMode mode = GuiViewer $ modify $ \st -> st { searchMode = mode }
    getSearchMode = GuiViewer $ gets searchMode
    openDeclaration di = do
        isDuplicate <- declarationIsOpen di
        when (not isDuplicate) $ do
            GuiViewer $ modify $ \s -> s
                { openDeclarations = di : openDeclarations s 
                , declarationHistory = History.singleton (di,"")
                }
    closeDeclaration di = GuiViewer $ do
        modify $ \s -> s
            { openDeclarations = delete di $ openDeclarations s 
            }
    getOpenDeclarations = GuiViewer $ gets openDeclarations
    declarationIsOpen di = GuiViewer $ gets $ (di `elem`) . openDeclarations
    openDeclarationInHistory di text = GuiViewer $ do
        modify $ \s -> s
            { openDeclarations = case () of
                ()
                    | di `elem` openDeclarations s -> openDeclarations s
                    | otherwise -> di : openDeclarations s
            , declarationHistory = History.insertBack (di,text) $ History.abandonFuture $ declarationHistory s
            }
    replaceHistoryPath di' = GuiViewer $ do
        history <- gets declarationHistory
        case History.present history of
            Nothing -> return ()
            Just (di,text) -> 
                case History.replace (di',text) history of
                    Just history' -> do
                        modify $ \s -> s{ declarationHistory = history' }
                    Nothing -> return ()
    replaceHistoryText text' = GuiViewer $ do
        history <- gets declarationHistory
        case History.present history of
            Nothing -> return ()
            Just (di,text) -> 
                case History.replace (di,text') history of
                    Just history' -> do
                        modify $ \s -> s{ declarationHistory = history' }
                    Nothing -> return ()    
    navigateHistoryBack = GuiViewer $ do
        history <- gets declarationHistory
        case History.back history of
            Just history' -> case History.present history' of
                Just di -> do
                    modify $ \s -> s{ declarationHistory = history' }
                    return $ Just di
                Nothing -> return Nothing
            Nothing -> return Nothing
    navigateHistoryForward = GuiViewer $ do
        history <- gets declarationHistory
        case History.forward history of
            Just history' -> case History.present history' of
                Just di -> do
                    modify $ \s -> s{ declarationHistory = history' }
                    return $ Just di
                Nothing -> return Nothing
            Nothing -> return Nothing

instance PseudoStateT GuiViewerT GuiViewerState where
    runPseudoStateT f s = runStateT (runGuiViewer f) s

instance MonadBounce GuiViewerT where
    bounce = ExceptT . lift . runExceptT

instance (ViewerStateClass m) => ViewerStateClass (GuiViewerT m) where
    getCurrentProject = lift getCurrentProject
    getCurrentModule = lift getCurrentModule
    getCurrentDeclaration = lift getCurrentDeclaration
    setCurrentProject = lift .-. setCurrentProject
    setCurrentModule = lift .-.. setCurrentModule
    setCurrentDecl = lift .-... setCurrentDecl
    setNoCurrentDecl = lift setNoCurrentDecl

instance (PersistenceClass m) => PersistenceClass (GuiViewerT m) where
    load = bounce load
    new = bounce . new
    finalize = bounce finalize

instance  (SolutionClass m) => SolutionClass (GuiViewerT m) where
    editSolutionInfo = bounce . editSolutionInfo
    addProject = bounce . addProject
    removeProject = bounce . removeProject
    getProjects = bounce getProjects
    editProjectInfo x = bounce . editProjectInfo x

instance  (ProjectModuleClass m) => ProjectModuleClass (GuiViewerT m) where
    --addModule x = bounce . addModule x
    createModule x = bounce . createModule x
    removeModule x = bounce . removeModule x
    --getModule x = bounce . getModule x
    getModules = bounce . getModules
    --editModule x y = bounce . editModule x y
    getModuleHeader x = bounce . getModuleHeader x
    editModuleHeader x y = bounce . editModuleHeader x y

instance   (ProjectExternModuleClass m) => ProjectExternModuleClass (GuiViewerT m) where
    --addExternModule x = bounce . addExternModule x
    createExternModule x = bounce . createExternModule x
    --getExternModule x = bounce . getExternModule x
    getExternModules = bounce . getExternModules
    removeExternModule x = bounce . removeExternModule x

instance   (ModuleDeclarationClass m) => ModuleDeclarationClass (GuiViewerT m) where
    editDeclaration x y z = bounce . editDeclaration x y z
    addDeclaration x y = bounce . addDeclaration x y
    getDeclaration x y = bounce . getDeclaration x y
    getDeclarations x = bounce . getDeclarations x
    removeDeclaration x y = bounce . removeDeclaration x y

instance   (ModuleImportClass m) => ModuleImportClass (GuiViewerT m) where
    addImport x y = bounce . addImport x y
    getImport x y = bounce . getImport x y
    removeImport x y = bounce . removeImport x y
    getImports x = bounce . getImports x

instance   (ModuleExportClass m) => ModuleExportClass (GuiViewerT m) where
    addExport x y = bounce . addExport x y
    getExport x y = bounce . getExport x y
    removeExport x y = bounce . removeExport x y
    exportAll x = bounce . exportAll x
    exportNothing x = bounce . exportNothing x
    getExports x = bounce . getExports x

instance   (ModulePragmaClass m) => ModulePragmaClass (GuiViewerT m) where
    addPragma x y = bounce . addPragma x y
    removePragma x y = bounce . removePragma x y
    getPragmas x = bounce . getPragmas x

instance (ExternModuleExportClass m) => ExternModuleExportClass (GuiViewerT m) where
    addExternExport x y = bounce . addExternExport x y
    getExternExport x y = bounce . getExternExport x y
    getExternExports x = bounce . getExternExports x
    removeExternExport x y = bounce . removeExternExport x y

instance (ModuleLocationClass m) => ModuleLocationClass (GuiViewerT m) where
    getModuleItemAtLocation = bounce .-... getModuleItemAtLocation
