{-|
Module      : Ide3.NewerMonad.Instances.State.ProjectExternModuleClass
Description : Stateful implementation of the ProjectExternModuleClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ide3.NewerMonad.Instances.State.ProjectExternModuleClass where

import Control.Monad.Except

import Ide3.Utils
import Ide3.NewerMonad
import Ide3.NewerMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types

-- | Access external modules statefully
instance ( StatefulSolutionClass u (t m)
         , MonadError (SolutionError u) (t m)
         )
      => ProjectExternModuleClass u (t (StatefulWrapper m)) where
    createExternModule = 
        modifySolutionER .-.. runDescent3 Solution.createExternModule
    getExternModules = 
        modifySolutionER .-. runDescent2 Solution.getExternModules
    removeExternModule = 
        modifySolutionER .-.. runDescent3 Solution.removeExternModule
