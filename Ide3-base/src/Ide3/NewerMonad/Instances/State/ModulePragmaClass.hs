{-|
Module      : Ide3.NewerMonad.Instances.State.ModulePragmaClass
Description : Stateful implementation of the ModulePragmaClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ide3.NewerMonad.Instances.State.ModulePragmaClass where

import Control.Monad.Except

import Ide3.Utils
import Ide3.NewerMonad
import Ide3.NewerMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types

-- | Access pragmas statefully
instance ( StatefulSolutionClass u (t m)
         , MonadError (SolutionError u) (t m)
         )
      => ModulePragmaClass u (t (StatefulWrapper m)) where
    addPragma = modifySolutionER .-... runDescent4 Solution.addPragma
    removePragma = modifySolutionER .-... runDescent4 Solution.removePragma
    getPragmas = modifySolutionER .-.. runDescent3 Solution.getPragmas
