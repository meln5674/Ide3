{-# LANGUAGE TypeFamilies, ConstraintKinds, KindSignatures, ConstrainedClassMethods, FlexibleContexts #-}
module GenericGuiEnv where

import GHC.Exts

import Control.Concurrent
import Control.Monad.Trans

import Ide3.Types

import GuiError

type UserError = GuiError

newtype IdleThreadTask = IdleThreadTask { getIdleThreadTask :: IO () }

class (MonadTrans t) => GenericGuiEnv t where
    type MonadConstraint t (m :: * -> *) :: Constraint
    type NewMonadConstraint t (m :: * -> *) :: Constraint
    type MonadType t :: (* -> *)
    dialogOnError :: (NewMonadConstraint t m, MonadConstraint t (MonadType t)) 
                  => t (MonadType t) a
                  -> t (SolutionResult UserError (MonadType t)) a
                  -> t m a
    dialogOnErrorConc :: (NewMonadConstraint t m, MonadConstraint t (MonadType t)) 
                  => t (MonadType t) ()
                  -> t (SolutionResult UserError (MonadType t)) ()
                  -> t m ThreadId
    addIdleTask :: (NewMonadConstraint t m, MonadConstraint t (MonadType t))
                => IdleThreadTask
                -> t m ()


dialogOnError' :: ( Monad (t (MonadType t))
                  , GenericGuiEnv t
                  , NewMonadConstraint t m
                  , MonadConstraint t (MonadType t)
                  ) 
               => a
               -> t (SolutionResult UserError (MonadType t)) a
               -> t m a
dialogOnError' = dialogOnError . return

dialogOnErrorConc' :: ( Monad (t (MonadType t))
                      , GenericGuiEnv t
                      , NewMonadConstraint t m
                      , MonadConstraint t (MonadType t)
                      ) 
                   => t (SolutionResult UserError (MonadType t)) ()
                   -> t m ThreadId
dialogOnErrorConc' = dialogOnErrorConc (return ())
