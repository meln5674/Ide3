{-|
Module      : Ide3.NewerMonad.Instances.State.ModuleExportClass
Description : Stateful implementation of the ModuleExportClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ide3.NewerMonad.Instances.State.ModuleExportClass where

import Control.Monad.Except

import Ide3.Utils
import Ide3.NewerMonad
import Ide3.NewerMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types

-- | Access exports statefully
instance ( StatefulSolutionClass u (t m)
         , MonadError (SolutionError u) (t m)
         )
      => ModuleExportClass u (t (StatefulWrapper m)) where
    addExport = modifySolutionER .-... runDescent4 Solution.addExport
    getExport = modifySolutionER .-... runDescent4 Solution.getExport
    removeExport = modifySolutionER .-... runDescent4 Solution.removeExport
    exportAll = modifySolutionER .-.. runDescent3 Solution.exportAll
    exportNothing = modifySolutionER .-.. runDescent3 Solution.exportNothing
    getExports = modifySolutionER .-.. runDescent3 Solution.getExports
