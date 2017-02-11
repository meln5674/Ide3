{-|
Module      : Ide3.NewerMonad.Instances.State.SolutionClass
Description : Stateful implementation of the SolutionClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ide3.NewerMonad.Instances.State.SolutionClass where

import Control.Monad.Except

import Ide3.Utils
import Ide3.NewerMonad
import Ide3.NewerMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types
import Ide3.Types.State


-- | Access solution info and projects statefull
instance ( StatefulSolutionClass u (t m) 
         , MonadError (SolutionError u) (t m)
         )
      => SolutionClass u (t (StatefulWrapper m)) where
    editSolutionInfo f = modifySolution 
                       $ \s -> s{ solutionInfo = f $ solutionInfo s }
    addProject = modifySolutionER .-. runDescent2 Solution.addProject
    removeProject = modifySolutionER .-. runDescent2 Solution.removeProject
    getProjects = modifySolutionER $ runDescent1 Solution.getProjects
    editProjectInfo = modifySolutionER .-.. runDescent3 Solution.editProjectInfo

