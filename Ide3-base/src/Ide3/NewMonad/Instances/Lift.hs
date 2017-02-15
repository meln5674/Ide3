{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Ide3.NewMonad.Instances.Lift where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Writer
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict

import Ide3.NewMonad.Instances.Lift.TH
import Ide3.NewMonad

import Ide3.Utils



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
ExceptT e;
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
MaybeT;
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
Lazy.StateT s;
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
Strict.StateT s;
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
Lazy.RWST r w s;
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
Strict.RWST r w s;
|]

