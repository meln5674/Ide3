{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ide3.Mechanism.State.Helpers where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict (StateT, runStateT, get, put)
import Control.Monad.Trans.Except
import Control.Monad.Identity

import Ide3.Monad
import Ide3.Types
import qualified Ide3.Project as Project

import Ide3.Mechanism.State.Types

getsProject :: ProjectStateM m => (Project -> a) -> m a
getsProject f = f <$> getProject

modifyProject :: ProjectStateM m => (Project -> Project) -> m ()
modifyProject f = liftM f getProject >>= putProject

modifyProjectE :: ProjectStateM m => (Project -> Either (ProjectError u) Project) -> ProjectResult m u ()
modifyProjectE f = modifyProjectER $ \p -> do
    p' <- f p
    return (p',())

modifyProjectER :: ProjectStateM m => (Project -> Either (ProjectError u) (Project,a)) -> ProjectResult m u a
modifyProjectER f = do
    p <- lift getProject
    case f p of
        Right (p',r) -> do lift $ putProject p'
                           return r
        Left msg -> throwE msg
