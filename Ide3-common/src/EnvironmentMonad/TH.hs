{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module EnvironmentMonad.TH where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict

import Ide3.NewMonad.Instances.Lift.TH
import Ide3.NewMonad.Instances.Lift

import EnvironmentMonad.Internal
import EnvironmentMonad.TH.Helpers
{-
instance (BuilderMonad m) => BuilderMonad (StateT s m) where
    getBuilder = (fmap (mapBuilder lift) . lift) getBuilder
-}

[betterderiving|
id;
;
  BuilderMonad
, RunnerMonad
, InitializerMonad
, SolutionEditorMonad
, ProjectInitializerMonad;
ExceptT e;
  getBuilder = liftBuilder
, getRunner = liftRunner
, getInitializer = liftInitializer
, getSolutionRetriever = liftSolutionRetriever
, getSolutionEditor = liftSolutionEditor
, getProjectInitializer = liftProjectInitializer
, getProjectEditor = liftProjectEditor
, getProjectRetriever =  liftProjectRetriever
, getProjectRemover = liftProjectRemover
|]

[betterderiving|
id;
;
  BuilderMonad
, RunnerMonad
, InitializerMonad
, SolutionEditorMonad
, ProjectInitializerMonad;
MaybeT;
  getBuilder = liftBuilder
, getRunner = liftRunner
, getInitializer = liftInitializer
, getSolutionRetriever = liftSolutionRetriever
, getSolutionEditor = liftSolutionEditor
, getProjectInitializer = liftProjectInitializer
, getProjectEditor = liftProjectEditor
, getProjectRetriever =  liftProjectRetriever
, getProjectRemover = liftProjectRemover
|]

[betterderiving|
id;
;
  BuilderMonad
, RunnerMonad
, InitializerMonad
, SolutionEditorMonad
, ProjectInitializerMonad;
ReaderT r;
  getBuilder = liftBuilder
, getRunner = liftRunner
, getInitializer = liftInitializer
, getSolutionRetriever = liftSolutionRetriever
, getSolutionEditor = liftSolutionEditor
, getProjectInitializer = liftProjectInitializer
, getProjectEditor = liftProjectEditor
, getProjectRetriever =  liftProjectRetriever
, getProjectRemover = liftProjectRemover
|]

[betterderiving|
id;
Monoid w;
  BuilderMonad
, RunnerMonad
, InitializerMonad
, SolutionEditorMonad
, ProjectInitializerMonad;
WriterT w;
  getBuilder = liftBuilder
, getRunner = liftRunner
, getInitializer = liftInitializer
, getSolutionRetriever = liftSolutionRetriever
, getSolutionEditor = liftSolutionEditor
, getProjectInitializer = liftProjectInitializer
, getProjectEditor = liftProjectEditor
, getProjectRetriever =  liftProjectRetriever
, getProjectRemover = liftProjectRemover
|]



[betterderiving|
id;
;
  BuilderMonad
, RunnerMonad
, InitializerMonad
, SolutionEditorMonad
, ProjectInitializerMonad;
Lazy.StateT s;
  getBuilder = liftBuilder
, getRunner = liftRunner
, getInitializer = liftInitializer
, getSolutionRetriever = liftSolutionRetriever
, getSolutionEditor = liftSolutionEditor
, getProjectInitializer = liftProjectInitializer
, getProjectEditor = liftProjectEditor
, getProjectRetriever =  liftProjectRetriever
, getProjectRemover = liftProjectRemover
|]

[betterderiving|
id;
;
  BuilderMonad
, RunnerMonad
, InitializerMonad
, SolutionEditorMonad
, ProjectInitializerMonad;
Strict.StateT s;
  getBuilder = liftBuilder
, getRunner = liftRunner
, getInitializer = liftInitializer
, getSolutionRetriever = liftSolutionRetriever
, getSolutionEditor = liftSolutionEditor
, getProjectInitializer = liftProjectInitializer
, getProjectEditor = liftProjectEditor
, getProjectRetriever =  liftProjectRetriever
, getProjectRemover = liftProjectRemover
|]



[betterderiving|
id;
Monoid w;
  BuilderMonad
, RunnerMonad
, InitializerMonad
, SolutionEditorMonad
, ProjectInitializerMonad;
Lazy.RWST r w s;
  getBuilder = liftBuilder
, getRunner = liftRunner
, getInitializer = liftInitializer
, getSolutionRetriever = liftSolutionRetriever
, getSolutionEditor = liftSolutionEditor
, getProjectInitializer = liftProjectInitializer
, getProjectEditor = liftProjectEditor
, getProjectRetriever =  liftProjectRetriever
, getProjectRemover = liftProjectRemover
|]

[betterderiving|
id;
Monoid w;
  BuilderMonad
, RunnerMonad
, InitializerMonad
, SolutionEditorMonad
, ProjectInitializerMonad;
Strict.RWST r w s;
  getBuilder = liftBuilder
, getRunner = liftRunner
, getInitializer = liftInitializer
, getSolutionRetriever = liftSolutionRetriever
, getSolutionEditor = liftSolutionEditor
, getProjectInitializer = liftProjectInitializer
, getProjectEditor = liftProjectEditor
, getProjectRetriever =  liftProjectRetriever
, getProjectRemover = liftProjectRemover
|]

