{-|
Module      : Ide3.Env
Description : Utility wrapper for solution data sturcture operations
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

Due to the chain of operations that are necessary for modifying the solution
data structures, the code for this contains a great deal of boilerplate.

This module provides typeclasses which wrap common operations
(lookup, updating, etc) of these structures and provide a way to realize this
code as a stack of monad transformers. The head of this stack is a StateT, the
state type being the data structure in question. Beneath this is some number of
ReaderT's, the environment types being keys for accessing the child values. 
Finally, the bottom of this stack is an ExceptT with the solution error as the
exception type. 

Ex: StateT Solution (ReaderT ProjectInfo 
                             (ReaderT ModuleInfo 
                                      (ReaderT String ...)

Would be an operation acting on a Solution, with keys for a project and a module
in that project, and finally a string as an argument to the final operation.

The descend* functions take one of these stacks, as well as a related stack
which has the top removed, and the top ReaderT replaced with a StateT with the
environment type instead of the key type.

Ex. The stack above would be paired with a:
StateT Project (ReaderT ModuleInfo (ReaderT String ...)

The descend* function looks up the Project pointed at by the ProjectInfo
contained in the ReaderT in the first stack, throws an exception if its not
found, then applies the state transformation described by the second stack, then
updates the Solution in the first stack's StateT to contain the new Project.

The descend* functions can then be chained to an (eventually) arbitrary depth.

TODO: The current implementation is limited in that it can only handle a finite
depth. This is because the "stack" is specified at the top and somewhere in the
middle, making it not a stack at all. This means that each level of descent
needs a different function.

At the moment I have absolutely no idea how to fix this.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ide3.Env where

import Data.Functor.Identity

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import Ide3.Types.State
import Ide3.Types.Internal hiding (getChild)

import qualified Ide3.Declaration as Declaration

-- | Types in this class can be used to produce a uniquely identifying key
class EnvParamClass env param | env -> param where
    -- | Produce the unique identifier for this value
    getParam :: env -> param

-- | A generalization of a container, parentEnv is a container of childEnv with
-- key type childParam. Operations may instead throw an exception of type e
class ParamEnvClass parentEnv childParam childEnv e where
    -- | Add a child
    addChildT :: Monad m 
              => childParam 
              -> childEnv 
              -> parentEnv 
              -> ExceptT e m parentEnv
    -- | Remove a child
    removeChildT :: Monad m 
                 => childParam 
                 -> parentEnv 
                 -> ExceptT e m (childEnv,parentEnv)
    -- | Lookup a child
    getChildT :: Monad m 
              => childParam 
              -> parentEnv 
              -> ExceptT e m childEnv
    -- | Update a child
    setChildT :: Monad m 
              => childParam 
              -> childParam 
              -> childEnv 
              -> parentEnv 
              -> ExceptT e m parentEnv

-- | Retreive project info
instance EnvParamClass Project ProjectInfo where
    getParam = projectInfo

-- | Retreive module info
instance EnvParamClass Module ModuleInfo where
    getParam = moduleInfo

-- | Retereieve external module info
instance EnvParamClass ExternModule ModuleInfo where
    getParam = externModuleInfo

-- | Retereive declaration info
instance EnvParamClass (WithBody Declaration) DeclarationInfo where
    getParam = Declaration.info . item

-- | Same as addChildT, but with no monad stack underneath
addChild :: ParamEnvClass parentEnv childParam childEnv e 
         => childParam
         -> childEnv
         -> parentEnv
         -> Either e parentEnv
addChild k v t = runIdentity $ runExceptT $ addChildT k v t

-- | Same as removeChildT, but with no monad stack underneath
removeChild :: ParamEnvClass parentEnv childParam childEnv e 
            => childParam
            -> parentEnv
            -> Either e (childEnv,parentEnv)
removeChild k t = runIdentity $ runExceptT $ removeChildT k t

-- | Same as getChildT, but with no monad stack underneath
getChild :: ParamEnvClass parentEnv childParam childEnv e 
         => childParam
         -> parentEnv
         -> Either e childEnv
getChild k t = runIdentity $ runExceptT $ getChildT k t

-- | Same as setChildT, but with no monad stack underneath
setChild :: ParamEnvClass parentEnv childParam childEnv e 
         => childParam
         -> childParam
         -> childEnv
         -> parentEnv
         -> Either e parentEnv
setChild k k' v t = runIdentity $ runExceptT $ setChildT k k' v t


-- | Take an environment operation over a child value type and turn it into a
-- stateful operation over its parent type with the child key type as an
-- environment.
-- The resulting operation looks up the child value using the key in its
-- environment and runs the original operation using the result
descendRO :: ( ParamEnvClass parentEnv childParam childEnv e 
             , Monad m
             )
          => ReaderT childEnv (ExceptT e m) a
          -> StateT parentEnv (ReaderT childParam (ExceptT e m)) a
descendRO f = do
    parentEnv <- get
    childParam <- lift ask
    childEnv <- lift $ lift $ getChildT childParam parentEnv
    lift $ lift $ runReaderT f childEnv

-- | Take a stateful operation over a child value type and turn it into a
-- stateful operation over its parent type with the child key type as an
-- environment.
-- The resulting operation looks up the child value using the key in its
-- environment and runs the original operation using the result, then
-- updates the parent type using the result of the child operation.
descend0 :: ( EnvParamClass childEnv childParam
            , ParamEnvClass parentEnv childParam childEnv e
            , Monad m
            )
        => StateT childEnv (ExceptT e m) a
        -> StateT parentEnv (ReaderT childParam (ExceptT e m)) a
descend0 f = do
    parentEnv <- get
    childParam <- lift ask
    childEnv <- lift $ lift $ getChildT childParam parentEnv
    (result,childEnv') <- lift $ lift $ runStateT f childEnv
    let childParam' = getParam childEnv'
    parentEnv' <- lift 
                $ lift 
                $ setChildT childParam childParam' childEnv' parentEnv
    put parentEnv'
    return result

-- | Same as descend0, but has an arbitrary transformer between the child
-- state transformer and the exception transformer
descend1 :: ( EnvParamClass childEnv childParam
            , MonadTrans t
            , Monad (t (ExceptT e m))
            , ParamEnvClass parentEnv childParam childEnv e
            , Monad m
            )
        => StateT childEnv (t (ExceptT e m)) a
        -> StateT parentEnv (ReaderT childParam (t (ExceptT e m))) a
descend1 f = do
    parentEnv <- get
    childParam <- lift ask
    childEnv <- lift $ lift $ lift $ getChildT childParam parentEnv
    (result,childEnv') <- lift $ lift $ runStateT f childEnv
    let childParam' = getParam childEnv'
    parentEnv' <- lift 
                $ lift 
                $ lift 
                $ setChildT childParam childParam' childEnv' parentEnv
    put parentEnv'
    return result

-- | Same as descend0, but has two arbitrary transformers between the child
-- state transformer and the exception transformer
descend2 :: ( EnvParamClass childEnv childParam
            , MonadTrans t
            , MonadTrans u
            , Monad (t (u (ExceptT e m)))
            , Monad (u (ExceptT e m))
            , Monad m
            , ParamEnvClass parentEnv childParam childEnv e
            )
        => StateT childEnv (t (u (ExceptT e m))) a
        -> StateT parentEnv (ReaderT childParam (t (u (ExceptT e m)))) a
descend2 f = do
    parentEnv <- get
    childParam <- lift ask
    childEnv <- lift $ lift $ lift $ lift $ getChildT childParam parentEnv
    (result,childEnv') <- lift $ lift $ runStateT f childEnv
    let childParam' = getParam childEnv'
    parentEnv' <- lift 
                $ lift 
                $ lift 
                $ lift 
                $ setChildT childParam childParam' childEnv' parentEnv
    put parentEnv'
    return result

-- | Same as descend0, but has three arbitrary transformers between the child
-- state transformer and the exception transformer
descend3 :: ( EnvParamClass childEnv childParam
            , MonadTrans t
            , MonadTrans u
            , MonadTrans v
            , Monad (t (u (v (ExceptT e m))))
            , Monad (u (v (ExceptT e m)))
            , Monad (v (ExceptT e m))
            , Monad m
            , ParamEnvClass parentEnv childParam childEnv e)
        => StateT childEnv (t (u (v (ExceptT e m)))) a
        -> StateT parentEnv (ReaderT childParam (t (u (v (ExceptT e m))))) a
descend3 f = do
    parentEnv <- get
    childParam <- lift ask
    childEnv <- lift 
              $ lift 
              $ lift 
              $ lift 
              $ lift 
              $ getChildT childParam parentEnv
    (result,childEnv') <- lift $ lift $ runStateT f childEnv
    let childParam' = getParam childEnv'
    parentEnv' <- lift 
                $ lift 
                $ lift 
                $ lift 
                $ lift 
                $ setChildT childParam childParam' childEnv' parentEnv
    put parentEnv'
    return result

-- | A stateful operation over a value
type DescentChain1 a m u = StateT a (SolutionResult u m)

-- | A stateful operation over a value with an environment with a key
type DescentChain2 a b m u = StateT a (ReaderT b (SolutionResult u m))

-- | A stateful operation over a value with an environment with two keys
type DescentChain3 a b c m u =
    StateT a (ReaderT b (ReaderT c (SolutionResult u m)))

-- | A stateful operation over a value with an environment with three keys
type DescentChain4 a b c d m u =
    StateT a (ReaderT b (ReaderT c (ReaderT d (SolutionResult u m))))

-- | A stateful operation over a value with an environment with four keys
type DescentChain5 a b c d e m u =
    StateT a
        (ReaderT b (ReaderT c (ReaderT d (ReaderT e (SolutionResult u m)))))

-- | Run a stateful operation over a value
runDescent1 :: DescentChain1 a m u r -> a -> SolutionResult u m (r,a)
runDescent1 = runStateT

-- | Run a stateful operation over a value with an environment with a key
runDescent2 :: Monad m 
            => DescentChain2 a b m u r 
            -> b 
            -> a 
            -> SolutionResult u m (r,a)
runDescent2 f b a = runReaderT (runStateT f a) b

-- | Run a stateful operation over a value with an environment with two keys
runDescent3 :: Monad m 
            => DescentChain3 a b c m u r 
            -> b 
            -> c 
            -> a 
            -> SolutionResult u m (r,a)
runDescent3 f b c a = runReaderT (runReaderT (runStateT f a) b) c

-- | Run a stateful operation over a value with an environment with three keys
runDescent4 :: Monad m 
            => DescentChain4 a b c d m u r 
            -> b 
            -> c 
            -> d 
            -> a 
            -> SolutionResult u m (r,a)
runDescent4 f b c d a =
    runReaderT (runReaderT (runReaderT (runStateT f a) b) c) d

-- | Run a stateful operation over a value with an environment with four keys
runDescent5 :: Monad m 
            => DescentChain5 a b c d e m u r 
            -> b 
            -> c 
            -> d 
            -> e 
            -> a 
            -> SolutionResult u m (r,a)
runDescent5 f b c d e a =
    runReaderT (runReaderT (runReaderT (runReaderT (runStateT f a) b) c) d) e

-- | Wrapper for throwing an exception
throw1 :: Monad m => SolutionError u -> DescentChain1 a m u r
throw1 = lift . throwE

-- | Wrapper for throwing an exception
throw2 :: Monad m => SolutionError u -> DescentChain2 a b m u r
throw2 = lift . lift . throwE

-- | Wrapper for throwing an exception
throw3 :: Monad m => SolutionError u -> DescentChain3 a b c m u r
throw3 = lift . lift . lift . throwE

-- | Wrapper for throwing an exception
throw4 :: Monad m => SolutionError u -> DescentChain4 a b c d m u r
throw4 = lift . lift . lift . lift . throwE

-- | Run a descent over a state and a environment with a list of keys
mapDescent2 :: Monad m 
            => DescentChain2 a b m u r 
            -> a 
            -> [b] 
            -> SolutionResult u m ([r],a)
mapDescent2 f a bs = runStateT rs a
  where
    g = runStateT f
    r b = StateT $ \a' -> runReaderT (g a') b
    rs = mapM r bs

-- | Same as mapDescent2, but discards the result of each operation
mapDescent2_ :: Monad m 
             => DescentChain2 a b m u r 
             -> a 
             -> [b] 
             -> SolutionResult u m a
mapDescent2_ f a bs = do
    (_,a') <- mapDescent2 f a bs
    return a'
