{-|
Module      : Ide3.NewMonad.Instances.State.ModulePragmaClass
Description : Stateful implementation of the ModulePragmaClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.NewMonad.Instances.State.ModulePragmaClass where

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

-- | Access pragmas statefully
instance StatefulSolutionClass m => ModulePragmaClass (StatefulWrapper m) where
    addPragma = modifySolutionER .-... runDescent4 Solution.addPragma
    removePragma = modifySolutionER .-... runDescent4 Solution.removePragma
    getPragmas = modifySolutionER .-.. runDescent3 Solution.getPragmas
