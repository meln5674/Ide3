{-|
Module      : Ide3.NewerMonad.Instances.State.ModuleDeclarationClass
Description : Stateful implementation of the ModuleDeclarationClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ide3.NewerMonad.Instances.State.ModuleDeclarationClass where

import Control.Monad.Except

import Ide3.Utils
import Ide3.NewerMonad
import Ide3.NewerMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types

-- | Access declarations statefully
instance ( StatefulSolutionClass u (t m)
         , MonadError (SolutionError u) (t m)
         )
      => ModuleDeclarationClass u (t (StatefulWrapper m)) where
    addDeclaration = modifySolutionER .-... runDescent4 Solution.addDeclaration
    getDeclaration = modifySolutionER .-... runDescent4 Solution.getDeclaration
    getDeclarations = modifySolutionER .-.. runDescent3 Solution.getDeclarations
    editDeclaration =
        modifySolutionER .-.... runDescent5 Solution.editDeclaration 
    removeDeclaration =
        modifySolutionER .-... runDescent4 Solution.removeDeclaration
