{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module EnvironmentMonad.Stack 
    ( module EnvironmentMonad.Stack
    , module EnvironmentMonad
    , module Builder.Stack
    , module Runner.Stack
    , module Initializer.Stack
    , module ProjectInitializer.Stack
    ) where

import Control.Monad.Catch

import Control.Monad.Trans

import Ide3.NewMonad

import CabalMonad

import EnvironmentMonad

import Builder.Stack
import Runner.Stack
import Initializer.Stack
import ProjectInitializer.Stack

import CabalFilesystemSolution

--deriving (MonadMask m) => MonadMask (StackEnvironment m)

{-
newtype StackEnvironment m a = StackEnvironment
    { runStackEnvironment :: m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadMask
           , MonadCatch
           , MonadThrow
           , CabalMonad
           , PersistenceClass
           , SolutionClass
           , ProjectModuleClass
           , ModuleImportClass
           , ModuleExportClass
           , ModuleDeclarationClass
           , ModulePragmaClass
           , ProjectExternModuleClass
           , ExternModuleExportClass
           , ModuleFileClass
           )
-}

instance (MonadIO m, MonadMask m) => BuilderMonad (CabalSolution m) where
    getBuilder = return stackBuilder

instance (MonadIO m, MonadMask m) => RunnerMonad (CabalSolution m) where
    getRunner = return stackRunner

instance (MonadIO m, MonadMask m) => InitializerMonad (CabalSolution m) where
   type ArgType (CabalSolution m) = StackInitializerArgs
   getInitializer = return stackInitializer 

instance (MonadIO m, MonadMask m, SolutionMonad m, ModuleFileClass m) 
        => ProjectInitializerMonad (CabalSolution m) where
    type ProjectArgType (CabalSolution m) = StackProjectInitializerArgs'
    getProjectInitializer = return stackProjectInitializer
