{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Ide3.NewMonad.Instances.Lift where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS

import Ide3.NewMonad.Instances.Lift.TH
import Ide3.NewMonad

import Ide3.Utils

[betterderiving|
splice2;
Monoid w;
  PersistenceClass
, SolutionClass
, ProjectModuleClass
, ProjectExternModuleClass
, ModuleDeclarationClass
, ModuleImportClass
, ModuleExportClass
, ModulePragmaClass
, ExternModuleExportClass
, ModuleFileClass
, ModuleLocationClass;
WriterT w;
|]

[betterderiving|
splice2;
;
  PersistenceClass
, SolutionClass
, ProjectModuleClass
, ProjectExternModuleClass
, ModuleDeclarationClass
, ModuleImportClass
, ModuleExportClass
, ModulePragmaClass
, ExternModuleExportClass
, ModuleFileClass
, ModuleLocationClass;
StateT s;
|]

[betterderiving|
splice2;
;
  PersistenceClass
, SolutionClass
, ProjectModuleClass
, ProjectExternModuleClass
, ModuleDeclarationClass
, ModuleImportClass
, ModuleExportClass
, ModulePragmaClass
, ExternModuleExportClass
, ModuleFileClass
, ModuleLocationClass;
ReaderT r;
|]

[betterderiving|
splice2;
Monoid w;
  PersistenceClass
, SolutionClass
, ProjectModuleClass
, ProjectExternModuleClass
, ModuleDeclarationClass
, ModuleImportClass
, ModuleExportClass
, ModulePragmaClass
, ExternModuleExportClass
, ModuleFileClass
, ModuleLocationClass;
RWST r w s;
|]
