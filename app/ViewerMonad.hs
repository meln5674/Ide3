{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ViewerMonad where


import Ide3.Monad (ProjectResult)
import Ide3.Mechanism.State (ProjectStateM, ProjectShellM)
--import Ide3.Mechanism.State

class (ProjectStateM m, ProjectShellM m, Monad m) => ViewerMonad m where
    setFileToOpen :: FilePath -> ProjectResult m u ()
    setDirectoryToOpen :: FilePath -> ProjectResult m u ()
    setTargetPath ::  String -> ProjectResult m u ()
    hasOpenedProject :: m Bool
    --saveCurrentProject :: ProjectResult m u ()
    
