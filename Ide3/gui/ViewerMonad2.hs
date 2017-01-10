{-|
Module      : ViewerMonad2
Description : Monads which can be 'interupted' in the IO monad
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

The classes in this module propose a solution to the problem that arises from
trying to use StateT to thread state through an application which uses callbacks
which are required to be IO actions. 

@
connect :: Object -> IO () -> IO ()

handler :: IO ()

statefulHandler :: StateT IO ()

addConnections = do
    -- Works
    connect object handler

    -- Type error
    connect object statefulHandler
@

The first step in this solution is to take a step back and consider a simpler
case of trying to thread a static environment through the same sort of
application. This can be accomplished through simple function application

@
envHandler :: Env -> IO ()

addConnections = do
    env <- makeEnv
    connect object (envHandler env)
@

This can be refactored to use ReaderT

@
readerTHandler :: ReaderT Env IO ()

addConnections = do
    env <- makeEnv
    connect object $ runReaderT readerTHandler env
@

One can then lift addConnections into the ReaderT to replace its dependence on makeEnv

@
addConnections = do
    env <- ask
    lift $ connect $ runReaderT readerTHandler env
@

This pattern can then be abstracted into a function

@
connectWithEnv :: Object -> ReaderT env IO () -> ReaderT env IO ()
connectWitEnv object handler = do
    env <- ask
    lift $ connect $ runReaderT handler env

addConnections = do
    connectWithEnv object readerTHandler
@

Which follows the same pattern as the original, environment-less, call

We are now able to thread a static environment through callbacks, but the problem
of threading state remains. One solution is to resort to mutable references, in
this case, MVars.


@
statefulHandler :: StateT state IO ()

addConnections mvar = do
    connect object $ do
        initialState <- takeMVar mvar
        (_,finalState) <- runStateT statefulHandler initialState
        putMVar mvar
@

This pattern, too, can be abstracted into a function

@
connectStateful :: MVar state -> Object -> StateT state IO () -> IO ()
connectStateful mvar object handler = do
    connect object $ do
        initialState <- takeMVar mvar
        (_,finalState) <- runStateT statefulHandler initialState
        putMVar mvar

addConnections mvar = do
    connectStateful mvar object statefulHandler

@

But this is no more than a ReaderT with a (Mvar state) as its environment!

@
connectStateful :: Object -> StateT state IO () -> ReaderT (MVar state) IO ()

object handler = do
    mvar <- ask
    lift $ connect object $ do
        initialState <- takeMVar mvar
        (_,finalState) <- runStateT statefulHandler initialState
        putMVar mvar
addConnections = do
    connectStateful object statefulHandler
@

And finally, we have an abstraction which allows us to thread state through
callbacks, unaware that their state is actually backed by a mutable reference.
This allows the creation of the rest of the application as though it were a
typical pure program.

However, this leaves much to be desired, what of nested state,
StateT outer (StateT inner IO) ()? This can be solved with a mutable reference
of a tuple of each state type.

@
connectNestedState :: Object -> StateT outer (StateT inner IO) () -> ReaderT (MVar (outer,inner)) ()
connectNestedState object handler = do
    mvar <- ask
    lift $ connect object $ do
        (initialOuter,initialInner) <- takeMVar mvar
        ((_,finalOuter),finalInner) <- runStateT (runStateT handler initialOuter) initialInner
        putMVar mvar (finalOuter, finalInner)
@

But this is cumbersome, as we would need to define a connect function for each
level of nesting, impossible, as there are infinitely many.

The solution is typeclasses, which are the contents of this module.

This module introduces the concept of the InteruptMonad, and its 3 variants:

- InteruptMonad0 : Monads which can be directly mapped into IO, e.g. IO, IdentityT IO

- InteruptMonad1: Monads which can be mapped into IO if given a parameter, e.g. ReaderT IO

- InteruptMonad2: Monads which can be mapped into IO if given a parameter, but
    also return an extra value, e.g. StateT IO

With these, the following combinations occur:

If m is an InteruptMonad2 with state type inner, then StateT outer m is an
    InteruptMonad2 with state type (outer, inner)

If m is an InteruptMonad2 with state type state, then ReaderT (MVar state) m is
    an InteruptMonad1

With these two, an stack of an abitrary number of StateT, capped with a ReaderT
    with a mutatble reference to a product of their state types as its
    environment can be used to thread state through callbacks.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleContexts #-}
module ViewerMonad2 where

import Control.Monad.Trans.Identity

import Control.Concurrent.MVar


import PseudoState
import PseudoReader

-- | Class of monads whose actions can be directly mapped into the IO monad
class (Monad m) => InteruptMonad0 m where
    -- | Map an action into the IO monad
    interupt0 :: m a -> IO a

-- | Class of monads whose actions can be mapped into the IO monad given an 
-- argument
class (Monad m) => InteruptMonad1 x m where
    -- | Map an action into the IO monad
    interupt1 :: x -> m a -> IO a

-- | Class of monads whose actions can be mapped into the IO monad given an
-- argument and returning an aditional result
class (Monad m) => InteruptMonad2 x m where
    -- | Map an action into the IO monad
    interupt2 :: x -> m a -> IO (a, x)

instance InteruptMonad0 IO where
    interupt0 = id

instance InteruptMonad0 m => InteruptMonad0 (IdentityT m) where
    interupt0 f = interupt0 $ runIdentityT f

{-
instance (InteruptMonad0 m) => InteruptMonad1 env (ReaderT env m) where
    interupt1 env f = interupt0 $ runReaderT f env

instance (InteruptMonad0 m) => InteruptMonad2 state (StateT state m) where
    interupt2 s f = interupt0 $ runStateT f s


instance (InteruptMonad2 state (StateT state m)) => InteruptMonad1 (MVar state) (StateT state m) where
    interupt1 mvar f = do
        s <- takeMVar mvar
        (x,s') <- interupt2 s f
        putMVar mvar s'
        return x

instance (InteruptMonad2 inner m) => InteruptMonad2 (outer,inner) (StateT outer m) where
    interupt2 (outer,inner) f = do
        ((x,outer'),inner') <- interupt2 inner $ runStateT f outer
        return (x,(outer',inner'))
-}


-- | A reader can wrap something which needs no arguments
instance {-# OVERLAPPABLE #-} 
         (Monad (t m), PseudoStateT t s, InteruptMonad0 m) 
      => InteruptMonad1 (MVar s) (t m) where
    interupt1 var f = do  
        s <- takeMVar var
        (x,s') <- interupt0 $ runPseudoStateT f s
        putMVar var s'
        return x

-- | A state can wrap something which needs no arguments
instance {-# OVERLAPPABLE #-}
         (Monad (t m), PseudoStateT t s, InteruptMonad0 m) 
      => InteruptMonad2 s (t m) 
 where
    interupt2 s f = interupt0 $ runPseudoStateT f s 

-- | A state can become a reader by accepting its argument as a mutable reference
instance {-# OVERLAPPABLE #-}
         (Monad (t m), PseudoStateT t s, InteruptMonad2 s' m) 
      => InteruptMonad1 (MVar (s,s')) (t m) where
    
    interupt1 var f = do
        (s,s2) <- takeMVar var
        ((x,s'),s2') <- interupt2 s2 $ runPseudoStateT f s
        putMVar var (s',s2')
        return x

-- | Two readers can stack on top of eachother by combining their environments in a tuple
instance {-# OVERLAPPABLE #-}
         (Monad (t m), PseudoReaderT t r1, InteruptMonad1 r2 m) 
      => InteruptMonad1 (r1,r2) (t m) where
    interupt1 (r1,r2) f = interupt1 r2 $ runPseudoReaderT f r1

-- | Two states can statck on top of eachother by combining their states in a tuple
instance {-# OVERLAPPABLE #-}
         (Monad (t m), PseudoStateT t s, InteruptMonad2 s' m) 
      => InteruptMonad2 (s,s') (t m) where
    interupt2 (s,s2) f = do
        ((x,s'),s2') <- interupt2 s2 $ runPseudoStateT f s
        return (x,(s',s2'))

{-
instance (Monad (t m), PseudoReaderT t r, InteruptMonad2 s m) => InteruptMonad1 (r, MVar s) (t m) where
    interupt1 (r,var) f = interupt1 var $ runPseudoReaderT f r
-}
