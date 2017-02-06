{-|
Module      : Ide3.NewMonad.Instances.State.ProjectModuleClass
Description : Stateful implementation of the ProjectModuleClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.NewMonad.Instances.State.ProjectModuleClass where

import Control.Lens

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
--import qualified Ide3.Env.Solution as Solution

import qualified Ide3.Solution.Lens as Solution
import qualified Ide3.Project.Lens as Project
import qualified Ide3.Module.Lens as Module

import qualified Ide3.Module as Module (new)

import Ide3.Types.State

-- | Access modules statefully
instance StatefulSolutionClass m => ProjectModuleClass (StatefulWrapper m) where
{-
    createModule = modifySolutionER .-.. runDescent3 Solution.createModule
    getModules = modifySolutionER .-. runDescent2 Solution.allModules
    removeModule = modifySolutionER .-.. runDescent3 Solution.removeModule
    getModuleHeader = modifySolutionER .-.. runDescent3 Solution.getModuleHeader
    editModuleHeader =
        modifySolutionER .-... runDescent4 Solution.editModuleHeader
    setModuleUnparsable pji mi contents loc msg
        = modifySolutionER $ runDescent4 Solution.setModuleUnparsable pji mi (contents, loc, msg)
    setModuleParsable = modifySolutionER .-.. runDescent3 Solution.setModuleParsable
    getUnparsableModule = modifySolutionER .-.. runDescent3 Solution.getUnparsableModule
    refreshModule _ mi = return mi
-}
    createModule pji mi = checkThenDo
        (Solution.uncheckLocalModule (pji, mi))
        ( ix pji 
        . Project.overLocals 
        . at mi ?~ Module.new mi
        )
    getModules pji = checkThenGet
        (Solution.checkProject pji)
        (ix pji . Project.getLocalModules)
    removeModule pji mi = checkThenDo
        (Solution.checkLocalModule (pji, mi))
        (ix pji . Project.overLocals . at mi .~ Nothing)
    getModuleHeader pji mi = checkThenGet
        (Solution.checkLocalModule (pji, mi))
        (ix pji . Project.overLocals . ix mi . Module.asParsable . Module.moduleHeader)
    editModuleHeader pji mi f = checkThenDo
        (Solution.checkLocalModule (pji, mi))
        (Solution.overLocals . ix (pji, mi) . Module.asParsable . Module.moduleHeader %~ f)
    setModuleUnparsable pji mi t loc msg = checkThenDo
        (Solution.checkLocalModule (pji, mi))
        (Solution.overLocals . ix (pji, mi) .~ UnparsableModule mi t loc msg)
    setModuleParsable pji mi = checkThenDo
        (Solution.checkLocalModule (pji, mi))
        (Solution.overLocals . ix (pji, mi) .~ Module.new mi)
    getUnparsableModule pji mi = checkThenGet
        (Solution.checkLocalModule (pji, mi))
        (Solution.overLocals . ix (pji, mi) . Module.getUnparsable)
    refreshModule _ mi = return mi
