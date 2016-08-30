{-|
Module      : Ide3.NewMonad.Instances.Undecidable
Description : Experimental instances of the NewMonad typeclasses
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module was an attempt to remove boilerplate from creating new instances of
the NewMonad typeclasses. Often, the new instances were simply calling
`bounce $ method`. The goal was to find some way to automatically generate these
instances.

So far, this has not yet succeeded.
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.NewMonad.Instances.Undecidable where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Utils
import Ide3.NewMonad
    
newtype UndecidableWrapper (t :: (* -> *) -> * -> *) (m :: * -> *) (a :: *) where
    UndecidableWrapper :: { runUndecidableWrapper :: t m a } -> UndecidableWrapper t m a
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadBounce)

instance (MonadBounce t, PersistenceClass m, Monad (t m)) => PersistenceClass (UndecidableWrapper t m) where
    load = bounce load
    new = bounce .-. new
    finalize = bounce finalize

instance (MonadBounce t, SolutionClass m, Monad (t m)) => SolutionClass (UndecidableWrapper t m) where
    editSolutionInfo = bounce .-. editSolutionInfo
    addProject = bounce .-. addProject
    removeProject = bounce .-. removeProject
    getProjects = bounce getProjects
    editProjectInfo = bounce .-.. editProjectInfo

instance (MonadBounce t, ProjectModuleClass m, Monad (t m)) => ProjectModuleClass (UndecidableWrapper t m) where
    createModule = bounce .-.. createModule
    removeModule = bounce .-.. removeModule
    getModules = bounce .-. getModules
    getModuleHeader = bounce .-.. getModuleHeader
    editModuleHeader = bounce .-... editModuleHeader

instance (MonadBounce t, ProjectExternModuleClass m, Monad (t m)) => ProjectExternModuleClass (UndecidableWrapper t m) where
    createExternModule = bounce .-.. createExternModule
    getExternModules = bounce .-. getExternModules
    removeExternModule = bounce .-.. removeExternModule

instance (MonadBounce t, ModuleDeclarationClass m, Monad (t m)) => ModuleDeclarationClass (UndecidableWrapper t m) where
    editDeclaration = bounce .-.... editDeclaration
    addDeclaration = bounce .-... addDeclaration
    getDeclaration = bounce .-... getDeclaration
    getDeclarations = bounce .-.. getDeclarations
    removeDeclaration = bounce .-... removeDeclaration

instance (MonadBounce t, ModuleImportClass m, Monad (t m)) => ModuleImportClass (UndecidableWrapper t m) where
    addImport = bounce .-... addImport
    getImport = bounce .-... getImport
    removeImport = bounce .-... removeImport
    getImports = bounce .-.. getImports

instance (MonadBounce t, ModuleExportClass m, Monad (t m)) => ModuleExportClass (UndecidableWrapper t m) where
    addExport = bounce .-... addExport
    getExport = bounce .-... getExport
    removeExport = bounce .-... removeExport
    exportAll = bounce .-.. exportAll
    exportNothing = bounce .-.. exportNothing
    getExports = bounce .-.. getExports

instance (MonadBounce t, ModulePragmaClass m, Monad (t m)) => ModulePragmaClass (UndecidableWrapper t m) where
    addPragma = bounce .-... addPragma
    removePragma = bounce .-... removePragma
    getPragmas = bounce .-.. getPragmas
