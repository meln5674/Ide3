{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module EnvironmentMonad.TH.Helpers where

import Control.Monad.Trans

import Ide3.NewMonad

import EnvironmentMonad.Internal

liftBuilder :: (Monad m, MonadTrans t, Functor (t m)) => m (Builder m) -> t m (Builder (t m))
liftBuilder = fmap (mapBuilder lift) . lift

liftRunner :: (Monad m, MonadTrans t, Functor (t m)) => m (Runner m) -> t m (Runner (t m))
liftRunner = fmap (mapRunner lift) . lift

liftInitializer :: ( Monad m
                   , MonadTrans t
                   , Functor (t m)
                   , PersistToken m ~ PersistToken (t m)
                   ) => m (Initializer a m) -> t m (Initializer a (t m))
liftInitializer = fmap (mapInitializer lift) . lift

liftSolutionRetriever :: (Monad m, MonadTrans t, Functor (t m)) => m (SolutionRetriever a m) -> t m (SolutionRetriever a (t m))
liftSolutionRetriever = fmap (mapSolutionRetriever lift) . lift

liftSolutionEditor :: (Monad m, MonadTrans t, Functor (t m)) => m (SolutionEditor a m) -> t m (SolutionEditor a (t m))
liftSolutionEditor = fmap (mapSolutionEditor lift) . lift

liftProjectInitializer :: (Monad m, MonadTrans t, Functor (t m)) => m (ProjectInitializer a m) -> t m (ProjectInitializer a (t m))
liftProjectInitializer = fmap (mapProjectInitializer lift) . lift

liftProjectEditor :: (Monad m, MonadTrans t, Functor (t m)) => m (ProjectEditor a m) -> t m (ProjectEditor a (t m))
liftProjectEditor = fmap (mapProjectEditor lift) . lift

liftProjectRetriever :: (Monad m, MonadTrans t, Functor (t m)) => m (ProjectRetriever a m) -> t m (ProjectRetriever a (t m))
liftProjectRetriever = fmap (mapProjectRetriever lift) . lift

liftProjectRemover :: (Monad m, MonadTrans t, Functor (t m)) => m (ProjectRemover a m) -> t m (ProjectRemover a (t m))
liftProjectRemover = fmap (mapProjectRemover lift) . lift
