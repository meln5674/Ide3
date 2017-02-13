{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module EnvironmentMonad 
    ( module EnvironmentMonad
    , module Builder
    , module Runner
    , module Initializer
    , module ProjectInitializer
    , module ProjectEditor
    , module ProjectRetriever
    , module ProjectRemover
    , module SolutionEditor
    , module SolutionRetriever
    , module Args
    ) where

import Args

import Builder
import Runner
import Initializer
import SolutionEditor
import SolutionRetriever
import ProjectInitializer
import ProjectRetriever
import ProjectEditor
import ProjectRemover

class Monad m => BuilderMonad m where
    getBuilder :: m (Builder m)

class Monad m => RunnerMonad m where
    getRunner :: m (Runner m)

class Monad m => InitializerMonad m where
    type ArgType m
    getInitializer :: Args (ArgType m) => m (Initializer (ArgType m) m)

class Monad m => SolutionEditorMonad m where
    type SolutionEditArgType m
    getSolutionRetriever :: Args (SolutionEditArgType m) => m (SolutionRetriever (SolutionEditArgType m) m)
    getSolutionEditor :: Args (SolutionEditArgType m) => m (SolutionEditor (SolutionEditArgType m) m)

class Monad m => ProjectInitializerMonad m where
    type ProjectArgType m
    getProjectInitializer :: Args (ProjectArgType m) => m (ProjectInitializer (ProjectArgType m) m)
    getProjectEditor :: Args (ProjectArgType m) => m (ProjectEditor (ProjectArgType m) m)
    getProjectRetriever :: Args (ProjectArgType m) => m (ProjectRetriever (ProjectArgType m) m)
    getProjectRemover :: Args (ProjectArgType m) => m (ProjectRemover (ProjectArgType m) m)

type EnvironmentMonad m = 
    ( BuilderMonad m
    , RunnerMonad m
    , InitializerMonad m
    , SolutionEditorMonad m
    , ProjectInitializerMonad m
    )
