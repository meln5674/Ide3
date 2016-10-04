{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
module ViewerMonad2 where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Control.Concurrent.MVar


import PseudoState

{-
class ViewerMonad (t m) => ViewerMonad2 t m p | t -> p where
    runViewer :: t m a -> p -> m (a,p)
    initialState :: p
-}


{-
class (Monad m) => InteruptMonad0 m where
    interupt0 :: m a -> IO a

class (Monad m) => InteruptMonad1 m where
    type InteruptType1 m
    interupt1 :: InteruptType1 m -> m a -> IO a

class (Monad m) => InteruptMonad2 m where
    type InteruptType2 m
    interupt2 :: InteruptType2 m -> m a -> IO (a, InteruptType2 m)

instance InteruptMonad0 IO where
    interupt0 = id


instance InteruptMonad0 m => InteruptMonad1 (ReaderT r m) where
    type InteruptType1 (ReaderT r m) = r
    interupt1 r = interupt0 . runReaderT r

instance InteruptMonad1 m => InteruptMonad1 (ReaderT r m) where
    type InteruptType1 (ReaderT r m) = (r, InteruptType1 m)
    interupt1 (r1,r2) = interupt1 r2 . runReaderT r1

instance InteruptMonad2 m => InteruptMonad2 (ReaderT r m) where
    type InteruptType2 (ReaderT r m) = (r, InteruptType2 m)
    interupt2 (r1,r2) f = interupt2 r2 (runReaderT r1 f) >>= \(a,r2') -> return (a, (r1,r2'))

instance InteruptMonad0 m => InteruptMonad2 (StateT s m) where
    type InteruptType2 (StateT s m) = s
    interupt2 = interupt0 . runStateT

instance InteruptMonad1 m => InteruptMonad2 (StateT s m) where
    type InteruptType2 (StateT s m) = (s, InteruptType1 m)
    interupt2 (s,r) f = interupt1 r (runStateT f s) >>= \(a,s') -> return (a,(s,r))

instance InteruptMonad2 m => InteruptMonad2 (StateT s m) where
    type InteruptType2 (StateT s m) = (s, InteruptType2 m)
    interupt2 (s1,s2) f = interupt2 s2 (runStateT f s1) >>= \((a,s2'),s1') -> return (a,(s2',s1'))

newtype ReaderT' r m a = ReaderT' { runReaderT' :: ReaderT (MVar r) m a }

newtype StateT' r m a = StateT' { runStateT' :: StateT (MVar r) m a }

instance InteruptMonad0 m => InteruptMonad1 (ReaderT' r m) where
    type InteruptType1 (ReaderT' r m) = MVar r
    interupt1 var f = do
        r <- takeMVar var
        a <- interupt0 $ runReaderT (runReaderT' f) r
        putMVar var r
        return a

instance InteruptMonad1 m => InteruptMonad1 (ReaderT' r m) where
    type InteruptType1 (ReaderT' r m) = MVar (r, InteruptType1 m)
    interupt1 var f = do
        (r1,r2) <- takeMVar var
        a <- interupt1 (runReaderT (runReaderT' f) r1) r2
        putMVar (r1, r2)
        return a

instance InteruptMonad2 m => InteruptMonad2 (ReaderT' r m) where
    type InteruptType2 (ReaderT' r m) = MVar (r, InteruptType2 m)
    interupt2 var f = do
        (r,s) <- takeMVar var
        (a,s') <- interupt2 (runReaderT (runReaderT' f) r) s
        putMVar (r,s')
        return a

instance InteruptMonad0 m => InteruptMonad2 (StateT' s m) where
    type InteruptType2 (StateT' s m) = MVar s
    interupt2 var f = do
        s <- takeMVar var
        (a,s') <- interupt0 $ runStateT (runStateT' f) s
        putMVar s'
        return a

instance InteruptMonad1 m => InteruptMonad2 (StateT' s m) where
    type InteruptType2 (StateT' s m) = MVar (s, InteruptType1 m)
    interupt2 var f = do
        (s,r) <- takeMVar var
        (a,s') <- interupt1 (runStateT (runStateT' f) s) r
        putMVar (s',r)
        return a

instance InteruptMonad2 m => InteruptMonad2 (StateT' s m) where
    type InteruptType2 (StateT' s m) = MVar (s, InteruptType2 m)
    interupt2 var f = do
        (s1,s2) <- takeMVar var
        ((a,s1'),s2') <- interupt2 (runStateT (runStateT' f) s1) s2
        putMVar (s1',s2')
        return a

-}

class (Monad m) => InteruptMonad0 m where
    interupt0 :: m a -> IO a

class (Monad m) => InteruptMonad1 x m where
    interupt1 :: x -> m a -> IO a

class (Monad m) => InteruptMonad2 x m where
    interupt2 :: x -> m a -> IO (a, x)

instance InteruptMonad0 IO where
    interupt0 = id

{-
instance InteruptMonad1 a IO where
    interupt1 _ f = f

instance InteruptMonad2 a IO where
    interupt2 x f = do
        a <- f
        return (a,x)
-}

instance (Monad (t m), PseudoStateT t s, InteruptMonad0 m) => InteruptMonad1 (MVar s) (t m) where
    interupt1 var f = do  
        s <- takeMVar var
        (x,s') <- interupt0 $ runPseudoStateT f s
        putMVar var s'
        return x

instance (Monad (t m), PseudoStateT t s, InteruptMonad0 m) => InteruptMonad2 s (t m) where
    interupt2 s f = interupt0 $ runPseudoStateT f s 

instance (Monad (t m), PseudoStateT t s, InteruptMonad2 s' m) => InteruptMonad1 (MVar (s,s')) (t m) where
    interupt1 var f = do
        putStrLn $ "Taking MVar"
        (s,s2) <- takeMVar var
        ((x,s'),s2') <- interupt2 s2 $ runPseudoStateT f s
        putStrLn $ "Replacing MVar"
        putMVar var (s',s2')
        return x

instance (Monad (t m), PseudoStateT t s, InteruptMonad2 s' m) => InteruptMonad2 (s,s') (t m) where
    interupt2 (s,s2) f = do
        ((x,s'),s2') <- interupt2 s2 $ runPseudoStateT f s
        return (x,(s',s2'))

