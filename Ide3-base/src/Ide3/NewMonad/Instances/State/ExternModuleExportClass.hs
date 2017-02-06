{-|
Module      : Ide3.NewMonad.Instances.State.ExternModuleExportClass
Description : Stateful implementation of the ExternModuleExportClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.NewMonad.Instances.State.ExternModuleExportClass where

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution
--import qualified Ide3.Solution.Lens as Solution

-- | Access external exports statefully
instance StatefulSolutionClass m 
      => ExternModuleExportClass (StatefulWrapper m) where
    addExternExport = 
        modifySolutionER .-... runDescent4 Solution.addExternExport
    getExternExport = 
        modifySolutionER .-... runDescent4 Solution.getExternExport 
    removeExternExport = 
        modifySolutionER .-... runDescent4 Solution.removeExternExport
    getExternExports = 
        modifySolutionER .-.. runDescent3 Solution.getExternExports

{-
instance StatefulSolutionClass m 
      => ExternModuleExportClass (StatefulWrapper m) where
    addExternExport pji mi e = do
        case Solution.checkModule pji mi of
            Just err -> throwE err
            Nothing -> do
-}
