{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ide3.Mechanism.State.Types 
    ( ProjectShellM (..)
    , ProjectStateM (..)
    , StatefulProject (..)
    , ProjectStateT (..)
    , ProjectState
    ) where


import Control.Monad.Trans
import Control.Monad.Trans.State.Strict (StateT, runStateT, get, put)
import Control.Monad.Trans.Except
import Control.Monad.Identity

import Ide3.Monad
import Ide3.Types
import qualified Ide3.Project as Project

newtype ProjectStateT m a = ProjectStateT { runProjectStateTInternal :: StateT Project m a }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    )

type ProjectState = ProjectStateT Identity


class Monad m => ProjectShellM m where
    load :: ProjectResult m u Project
    new :: ProjectInfo -> ProjectResult m u Project
    finalize :: Project -> ProjectResult m u ()

class Monad m => ProjectStateM m where
    getProject :: m Project
    putProject :: Project -> m ()


newtype StatefulProject m a = MkStatefulProject { runStatefulProject :: m a }
  deriving (Functor, Applicative, Monad, ProjectStateM, ProjectShellM, MonadIO)
