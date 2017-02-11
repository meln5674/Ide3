{-|
Module      : Ide3.NewerMonad.Instances.State.ProjectModuleClass
Description : Stateful implementation of the ProjectModuleClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ide3.NewerMonad.Instances.State.ProjectModuleClass where

import Control.Monad.Except

import Ide3.Utils
import Ide3.NewerMonad
import Ide3.NewerMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types

-- | Access modules statefully
instance ( StatefulSolutionClass u (t m)
         , MonadError (SolutionError u) (t (StatefulWrapper m))
         )
      => ProjectModuleClass u (t (StatefulWrapper m)) where
    createModule = modifySolutionER .-.. runDescent3 Solution.createModule
    getModules = modifySolutionER .-. runDescent2 Solution.allModules
    removeModule = modifySolutionER .-.. runDescent3 Solution.removeModule
    getModuleHeader = modifySolutionER .-.. runDescent3 Solution.getModuleHeader
    editModuleHeader =
        modifySolutionER .-... runDescent4 Solution.editModuleHeader
    setModuleUnparsable = modifySolutionER .-... runDescent4 Solution.setModuleUnparsable
    setModuleParsable = modifySolutionER .-.. runDescent3 Solution.setModuleParsable
    getUnparsableModule = modifySolutionER .-.. runDescent3 Solution.getUnparsableModule
    refreshModule _ mi = return mi
