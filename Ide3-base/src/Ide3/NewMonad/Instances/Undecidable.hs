{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ide3.NewMonad.Instances.Undecidable where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.NewMonad
    
newtype UndecidableWrapper (t :: (* -> *) -> * -> *) (m :: * -> *) (a :: *) where
    UndecidableWrapper :: { runUndecidableWrapper :: t m a } -> UndecidableWrapper t m a
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadBounce)

instance  (MonadBounce t, PersistenceClass m, Monad (t m)) => PersistenceClass (UndecidableWrapper t m) where
    load = bounce load
    new = bounce . new
    finalize = bounce finalize

instance  (MonadBounce t, SolutionClass m, Monad (t m)) => SolutionClass (UndecidableWrapper t m) where
    editSolutionInfo = bounce . editSolutionInfo
    addProject = bounce . addProject
    removeProject = bounce . removeProject
    getProjects = bounce getProjects
    editProjectInfo x = bounce . editProjectInfo x

instance  (MonadBounce t, ProjectModuleClass m, Monad (t m)) => ProjectModuleClass (UndecidableWrapper t m) where
    addModule x = bounce . addModule x
    createModule x = bounce . createModule x
    removeModule x = bounce . removeModule x
    getModule x = bounce . getModule x
    getModules = bounce . getModules
    editModule x y = bounce . editModule x y

instance   (MonadBounce t, ProjectExternModuleClass m, Monad (t m)) => ProjectExternModuleClass (UndecidableWrapper t m) where
    addExternModule x = bounce . addExternModule x
    getExternModule x = bounce . getExternModule x
    getExternModules = bounce . getExternModules
    removeExternModule x = bounce . removeExternModule x

instance   (MonadBounce t, ModuleDeclarationClass m, Monad (t m)) => ModuleDeclarationClass (UndecidableWrapper t m) where
    editDeclaration x y z = bounce . editDeclaration x y z
    addDeclaration x y = bounce . addDeclaration x y
    getDeclaration x y = bounce . getDeclaration x y
    getDeclarations x = bounce . getDeclarations x
    removeDeclaration x y = bounce . removeDeclaration x y

instance   (MonadBounce t, ModuleImportClass m, Monad (t m)) => ModuleImportClass (UndecidableWrapper t m) where
    addImport x y = bounce . addImport x y
    getImport x y = bounce . getImport x y
    removeImport x y = bounce . removeImport x y
    getImports x = bounce . getImports x

instance   (MonadBounce t, ModuleExportClass m, Monad (t m)) => ModuleExportClass (UndecidableWrapper t m) where
    addExport x y = bounce . addExport x y
    getExport x y = bounce . getExport x y
    removeExport x y = bounce . removeExport x y
    exportAll x = bounce . exportAll x
    exportNothing x = bounce . exportNothing x
    getExports x = bounce . getExports x

instance   (MonadBounce t, ModulePragmaClass m, Monad (t m)) => ModulePragmaClass (UndecidableWrapper t m) where
    addPragma x y = bounce . addPragma x y
    removePragma x y = bounce . removePragma x y
    getPragmas x = bounce . getPragmas x
