{-# LANGUAGE MultiParamTypeClasses #-}
module ViewerMonad where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Monad (ProjectM, ProjectResult)
--import Ide3.Mechanism.State

class ProjectM m => ViewerMonad m where
    setFileToOpen :: FilePath -> ProjectResult m u ()
    setDirectoryToOpen :: FilePath -> ProjectResult m u ()
    setTargetPath :: String -> ProjectResult m u ()
    hasOpenedProject :: m Bool
    --saveCurrentProject :: ProjectResult m u ()
