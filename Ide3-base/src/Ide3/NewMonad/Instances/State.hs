{-|
Module      : Ide3.NewMonad.Instances.State
Description : Stateful implementation of the NewMonad typeclasses
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides an implementation of the typeclasses in Ide3.NewMonad,
using the StateT monad transformer, and set of data types representing the tree
of projects and modules. 

The implementation is provided using two typeclasses of its own, one for
managing the in-memory structure of projects and modules, and the second for
performing persistence. The persistence class provides methods similar to those
in PersistenceClass, but explicitly take and return the in-memory structure.

To use this implementation, create an instance of StatefulSolutionClass (or use
either of the the ones provided at Ide3.NewMonad.Instances.State.Class.Lazy or
Ide3.NewMonad.Instances.State.Class.Struct), as well as an instance of
StatefulPeristenceClass for the same monad, `m`. Once implemented,
`StatefulWrapper m` will have an instance for each of the NewMonad typeclasses.


-}

module Ide3.NewMonad.Instances.State
    ( StatefulSolutionClass (..)
    , StatefulPersistenceClass (..)
    , StatefulWrapper (..)
    , Solution
    ) where

import Ide3.Types.State
import Ide3.NewMonad.Instances.State.Class

import Ide3.NewMonad.Instances.State.Compose()

import Ide3.NewMonad.Instances.State.SolutionClass()
import Ide3.NewMonad.Instances.State.ProjectModuleClass()
import Ide3.NewMonad.Instances.State.ProjectExternModuleClass()
import Ide3.NewMonad.Instances.State.ModuleDeclarationClass()
import Ide3.NewMonad.Instances.State.ModuleImportClass()
import Ide3.NewMonad.Instances.State.ModuleExportClass()
import Ide3.NewMonad.Instances.State.ModulePragmaClass()
import Ide3.NewMonad.Instances.State.ExternModuleExportClass()
import Ide3.NewMonad.Instances.State.ModuleFileClass()
--import Ide3.NewMonad.Instances.State.ModuleLocationClass()
