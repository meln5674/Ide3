{-|
Module      : Ide3.NewMonad.Instances.State.ModuleDeclarationClass
Description : Stateful implementation of the ModuleDeclarationClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.NewMonad.Instances.State.ModuleDeclarationClass where

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

-- |
instance StatefulSolutionClass m => ModuleDeclarationClass (StatefulWrapper m) where
    addDeclaration = modifySolutionER .-... runDescent4 Solution.addDeclaration
    getDeclaration = modifySolutionER .-... runDescent4 Solution.getDeclaration
    getDeclarations = modifySolutionER .-.. runDescent3 Solution.getDeclarations
    editDeclaration = modifySolutionER .-.... runDescent5 Solution.editDeclaration 
    removeDeclaration = modifySolutionER .-... runDescent4 Solution.removeDeclaration
