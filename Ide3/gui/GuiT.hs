{-# LANGUAGE TypeFamilies, ConstraintKinds, KindSignatures, ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GuiT where

import Data.Text

import Control.Concurrent

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Ide3.Utils

import Viewer
import ViewerMonad2

import GuiHelpers

import GuiClass
import GuiClass.GuiEnv()

import GuiEnv hiding (addIdleTask)
import qualified GuiEnv (addIdleTask)
import Dialogs
import GenericGuiEnv

newtype GuiT m' p m a = GuiT { runGuiTInternal :: GuiEnvT m' p (DialogsT m) a }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , SolutionViewClass
    , SearchBarClass
    , EditorBufferClass
    , ErrorListClass
    , BuildBufferClass
    )

instance MonadTrans (GuiT m' p) where
    lift = GuiT . lift . lift

instance MonadBounce (GuiT m' p) where
    bounce = ExceptT . lift . runExceptT

instance MonadSplice (GuiT m' p) where
    splice f = GuiT $ mkGuiEnvT $ \env -> mkDialogsT $ \dialogs -> lift $ runGuiT f env dialogs

instance MonadUnsplice (GuiT m' p) where
    unsplice f = GuiT $ mkGuiEnvT $ \env -> mkDialogsT $ \dialogs -> ExceptT $ runGuiT f env dialogs

instance SignalInterceptClass (GuiT m' p) where
    intercept add handler = do
        env <- liftEnv getEnv
        dialogs <- liftDialogs getDialogs
        lift $ add $ \x -> runGuiT (handler x) env dialogs

--instance DialogsClass (GuiT m' p) where
--    withMainWindow = liftDialogs . withMainWindow

{-
instance ( Monad m
         , SolutionInitializerClass' (GuiEnvT m' p (DialogsT m)) m
         , ArgType (ClassSolutionInitializerMonad (GuiEnvT m' p (DialogsT m))) 
            ~ ArgType m'
         )
        => SolutionInitializerClass (GuiT m' p m) where
    type ClassSolutionInitializerMonad (GuiT m' p m) 
--        = ClassSolutionInitializerMonad (GuiEnvT m' p (DialogsT m))
        = m'
    setupSolutionCreator = GuiT setupSolutionCreator
    getSolutionCreatorArg = GuiT getSolutionCreatorArg
    finalizeSolutionCreator = GuiT finalizeSolutionCreator
-}
    
liftEnv :: Monad m => GuiEnvT m' p m a -> GuiT m' p m a
liftEnv f = GuiT $ mkGuiEnvT $ \env -> lift $ runGuiEnvT f env

liftDialogs :: DialogsT m a -> GuiT m' p m a
liftDialogs f = GuiT $ GuiEnvT $ ReaderT $ const f

runGuiT :: GuiT m' p m a -> GuiEnv m' p -> Dialogs -> m a
runGuiT f env dialogs = runDialogsT (runGuiEnvT (runGuiTInternal f) env) dialogs

mkGuiT :: (GuiEnv m' p -> Dialogs -> m a) -> GuiT m' p m a
mkGuiT f = GuiT $ mkGuiEnvT $ \env -> mkDialogsT (f env)



type ThisMonadConstraint m' p m =
        ( ViewerMonad m'
        , InteruptMonad1 (MVar (MVarType p)) m'
        , ErrorClass m'
        , MonadIO m
        )
    
instance GenericGuiEnv (GuiT m' p) where
    type MonadConstraint (GuiT m' p) m = ThisMonadConstraint m' p m
    type NewMonadConstraint (GuiT m' p) m = (MonadIO m)
    type MonadType (GuiT m' p) = m'
    dialogOnError default_ f = do
        env <- liftEnv $ getEnv
        dialogs <- liftDialogs $ getDialogs
        liftEnv $  withSolutionMVar $ \var -> liftIO $ do
            interupt1 var $ do
                r <- runExceptT $ runGuiT f env dialogs
                case r of
                    Right x -> return x
                    Left e -> do
                        displayError $ pack $ show e
                        return default_

    dialogOnErrorConc f = do
        env <- liftEnv $ getEnv
        dialogs <- liftDialogs $ getDialogs
        liftEnv $ withSolutionMVar $ \var -> liftIO $ forkIO $ do
            interupt1 var $ do
                r <- runExceptT $ runGuiT f env dialogs
                case r of
                    Right () -> return ()
                    Left e -> do
                        displayError $ pack $ show $ e
    addIdleTask t = GuiT $ GuiEnv.addIdleTask t
