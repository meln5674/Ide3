{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module ViewerMonad2 where

import Control.Monad.Trans.State.Strict

import Control.Concurrent.MVar

import Viewer

{-
class ViewerMonad (t m) => ViewerMonad2 t m p | t -> p where
    runViewer :: t m a -> p -> m (a,p)
    initialState :: p
-}


class InteruptMonad0 m where
    interupt0 :: m a -> IO a

class InteruptMonad1 x m where
    interupt1 :: x -> m a -> IO a

class InteruptMonad2 x m where
    interupt2 :: x -> m a -> IO (a,x)

instance InteruptMonad0 IO where
    interupt0 = id

instance InteruptMonad1 a IO where
    interupt1 _ f = f

instance InteruptMonad2 a IO where
    interupt2 x f = do
        a <- f
        return (a,x)

instance InteruptMonad0 m => InteruptMonad1 (MVar s) (StateT s m) where
    interupt1 var f = do  
        s <- takeMVar var
        (x,s') <- interupt0 $ runStateT f s
        putMVar var s'
        return x

instance InteruptMonad0 m => InteruptMonad2 s (StateT s m) where
    interupt2 s f = interupt0 $ runStateT f s 
        
        

instance InteruptMonad2 s' m => InteruptMonad1 (MVar (s,s')) (StateT s m) where
    interupt1 var f = do
        (s,s2) <- takeMVar var
        ((x,s'),s2') <- interupt2 s2 $ runStateT f s
        putMVar var (s',s2')
        return x

instance InteruptMonad2 s' m => InteruptMonad2 (s,s') (StateT s m) where
    interupt2 (s,s2) f = do
        ((x,s'),s2') <- interupt2 s2 $ runStateT f s
        return (x,(s',s2'))
