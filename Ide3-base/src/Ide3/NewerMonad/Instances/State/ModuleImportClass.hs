{-|
Module      : Ide3.NewerMonad.Instances.State.ModuleImportClass
Description : Stateful implementation of the ModuleImportClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ide3.NewerMonad.Instances.State.ModuleImportClass where

import Control.Monad.Except

import Ide3.Utils
import Ide3.NewerMonad
import Ide3.NewerMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types

-- | Access imports statefully
instance ( StatefulSolutionClass u (t m)
         , MonadError (SolutionError u) (t m)
         )
      => ModuleImportClass u (t (StatefulWrapper m)) where
    addImport = modifySolutionER .-... runDescent4 Solution.addImport
    getImport = modifySolutionER .-... runDescent4 Solution.getImport
    removeImport = modifySolutionER .-... runDescent4 Solution.removeImport
    getImports = modifySolutionER .-.. runDescent3 Solution.getImports
