{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module GuiViewer where

import Data.List (delete)

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.NewMonad
import Ide3.Types

import PseudoState

import ViewerMonad

data GuiViewerState
    = GuiViewerState
    { openDeclarations :: [DeclarationInfo]
    }

emptyGuiViewer :: GuiViewerState
emptyGuiViewer = GuiViewerState []

newtype GuiViewer m a = GuiViewer { runGuiViewer :: StateT GuiViewerState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, ViewerMonad)

deriving instance (MonadMask m) => MonadMask (GuiViewer m)
deriving instance (MonadCatch m) => MonadCatch (GuiViewer m)
deriving instance (MonadThrow m) => MonadThrow (GuiViewer m)

openDeclaration :: Monad m => DeclarationInfo -> GuiViewer m ()
openDeclaration di = do
    isDuplicate <- declarationIsOpen di
    when (not isDuplicate) $ do
        GuiViewer $ modify $ \s -> s{ openDeclarations = di : openDeclarations s }

closeDeclaration :: Monad m => DeclarationInfo -> GuiViewer m ()
closeDeclaration di = GuiViewer $ do
    modify $ \s -> s{ openDeclarations = delete di $ openDeclarations s }

getOpenDeclarations :: Monad m => GuiViewer m [DeclarationInfo]
getOpenDeclarations = GuiViewer $ gets openDeclarations

declarationIsOpen :: Monad m => DeclarationInfo -> GuiViewer m Bool
declarationIsOpen di = GuiViewer $ gets $ (di `elem`) . openDeclarations

instance PseudoStateT GuiViewer GuiViewerState where
    runPseudoStateT f s = runStateT (runGuiViewer f) s

instance MonadBounce GuiViewer where
    bounce = ExceptT . lift . runExceptT

instance (PersistenceClass m) => PersistenceClass (GuiViewer m) where
    load = bounce load
    new = bounce . new
    finalize = bounce finalize

instance  (SolutionClass m) => SolutionClass (GuiViewer m) where
    editSolutionInfo = bounce . editSolutionInfo
    addProject = bounce . addProject
    removeProject = bounce . removeProject
    getProjects = bounce getProjects
    editProjectInfo x = bounce . editProjectInfo x

instance  (ProjectModuleClass m) => ProjectModuleClass (GuiViewer  m) where
    --addModule x = bounce . addModule x
    createModule x = bounce . createModule x
    removeModule x = bounce . removeModule x
    --getModule x = bounce . getModule x
    getModules = bounce . getModules
    --editModule x y = bounce . editModule x y
    getModuleHeader x = bounce . getModuleHeader x
    editModuleHeader x y = bounce . editModuleHeader x y

instance   (ProjectExternModuleClass m) => ProjectExternModuleClass (GuiViewer  m) where
    --addExternModule x = bounce . addExternModule x
    createExternModule x = bounce . createExternModule x
    --getExternModule x = bounce . getExternModule x
    getExternModules = bounce . getExternModules
    removeExternModule x = bounce . removeExternModule x

instance   (ModuleDeclarationClass m) => ModuleDeclarationClass (GuiViewer  m) where
    editDeclaration x y z = bounce . editDeclaration x y z
    addDeclaration x y = bounce . addDeclaration x y
    getDeclaration x y = bounce . getDeclaration x y
    getDeclarations x = bounce . getDeclarations x
    removeDeclaration x y = bounce . removeDeclaration x y

instance   (ModuleImportClass m) => ModuleImportClass (GuiViewer  m) where
    addImport x y = bounce . addImport x y
    getImport x y = bounce . getImport x y
    removeImport x y = bounce . removeImport x y
    getImports x = bounce . getImports x

instance   (ModuleExportClass m) => ModuleExportClass (GuiViewer  m) where
    addExport x y = bounce . addExport x y
    getExport x y = bounce . getExport x y
    removeExport x y = bounce . removeExport x y
    exportAll x = bounce . exportAll x
    exportNothing x = bounce . exportNothing x
    getExports x = bounce . getExports x

instance   (ModulePragmaClass m) => ModulePragmaClass (GuiViewer  m) where
    addPragma x y = bounce . addPragma x y
    removePragma x y = bounce . removePragma x y
    getPragmas x = bounce . getPragmas x

instance (ExternModuleExportClass m) => ExternModuleExportClass (GuiViewer m) where
    addExternExport x y = bounce . addExternExport x y
    getExternExport x y = bounce . getExternExport x y
    getExternExports x = bounce . getExternExports x
    removeExternExport x y = bounce . removeExternExport x y
