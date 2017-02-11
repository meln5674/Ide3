{-|
Module      : Ide3.NewerMonad.Instances.State.ExternModuleExportClass
Description : Stateful implementation of the ExternModuleExportClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ide3.NewerMonad.Instances.State.ExternModuleExportClass where

import Control.Monad.Except

import Ide3.Utils
import Ide3.NewerMonad
import Ide3.NewerMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types

-- | Access external exports statefully
instance ( StatefulSolutionClass u (t m)
         , MonadError (SolutionError u) (t m)
         )
      => ExternModuleExportClass u (t (StatefulWrapper m)) where
    addExternExport = 
        modifySolutionER .-... runDescent4 Solution.addExternExport
    getExternExport = 
        modifySolutionER .-... runDescent4 Solution.getExternExport 
    removeExternExport = 
        modifySolutionER .-... runDescent4 Solution.removeExternExport
    getExternExports = 
        modifySolutionER .-.. runDescent3 Solution.getExternExports

