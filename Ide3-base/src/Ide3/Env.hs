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

Ex: StateT Solution (ReaderT ProjectInfo (ReaderT ModuleInfo (ReaderT String ...)

Would be an operation acting on a Solution, with keys for a project and a module
in that project, and finally a string as an argument to the final operation.

The descend* functions take one of these stacks, as well as a related stack which
has the top removed, and the top ReaderT replaced with a StateT with the
environment type instead of the key type.

Ex. The stack above would be paired with a:
StateT Project (ReaderT ModuleInfo (ReaderT String ...)

The descend* function looks up the Project pointed at by the ProjectInfo
contained in the ReaderT in the first stack, throws an exception if its not found,
then applies the state transformation described by the second stack, then
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ide3.Env where

import qualified Data.Map as Map

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import Ide3.Types hiding (getChild)

import qualified Ide3.Declaration as Declaration


-- | Types in this class can be used to produce a uniquely identifying key
class EnvParamClass env param | env -> param where
    -- | Produce the unique identifier for this value
    getParam :: env -> param

-- | A generalization of a container, parentEnv is a container of childEnv with
-- key type childParam. Operations may instead throw an exception of type e
class ParamEnvClass parentEnv childParam childEnv e where
    -- | Add a child
    addChild :: Monad m => childParam -> childEnv -> parentEnv -> ExceptT e m parentEnv
    -- | Remove a child
    removeChild :: Monad m => childParam -> parentEnv -> ExceptT e m (childEnv,parentEnv)
    -- | Lookup a child
    getChild :: Monad m => childParam -> parentEnv -> ExceptT e m childEnv
    -- | Update a child
    setChild :: Monad m => childParam -> childParam -> childEnv -> parentEnv -> ExceptT e m parentEnv

instance EnvParamClass Project ProjectInfo where
    getParam = projectInfo

instance EnvParamClass Module ModuleInfo where
    getParam = moduleInfo

instance EnvParamClass ExternModule ModuleInfo where
    getParam = externModuleInfo

instance EnvParamClass (WithBody Declaration) DeclarationInfo where
    getParam = Declaration.info . item

instance ParamEnvClass Solution ProjectInfo Project (SolutionError u) where
    addChild pi p s = do
        case Map.lookup pi $ solutionProjects s of
            Just _ -> throwE $ DuplicateProject pi $ "Solution.addProject"
            Nothing -> return $ s{ solutionProjects = Map.insert pi p $ solutionProjects s }
    removeChild pi s = do
        case Map.lookup pi $ solutionProjects s of
            Nothing -> throwE $ ProjectNotFound pi $ "Solution.addProject"
            Just p -> return (p, s{ solutionProjects = Map.delete pi $ solutionProjects s })
    getChild pi s = do
        case Map.lookup pi $ solutionProjects s of
            Just p -> return p
            Nothing -> throwE undefined
    setChild pi pi' p' s = do
        case Map.lookup pi $ solutionProjects s of
            Just _ -> return $ s 
                { solutionProjects
                    = Map.insert pi' p' 
                    $ Map.delete pi 
                    $ solutionProjects s 
                }
            Nothing -> throwE undefined

instance ParamEnvClass Project ModuleInfo Module (SolutionError u) where
    addChild mi m p = do
        case Map.lookup mi $ projectModules p of
            Just _ -> throwE $ DuplicateModule mi "Project.addModule" 
            Nothing -> return $ p{ projectModules = Map.insert mi m $ projectModules p }
    removeChild mi p = do
        case Map.lookup mi $ projectModules p of
            Nothing -> throwE undefined
            Just m -> return (m, p{ projectModules = Map.delete mi $ projectModules p })
    getChild mi p = do
        case Map.lookup mi $ projectModules p of
            Just m -> return m
            Nothing -> throwE undefined
    setChild mi mi' m' p = do
        case Map.lookup mi $ projectModules p of
            Just _ -> return $ p
                { projectModules
                    = Map.insert mi' m' 
                    $ Map.delete mi 
                    $ projectModules p 
                }
            Nothing -> throwE undefined

instance ParamEnvClass Project ModuleInfo ExternModule (SolutionError u) where
    addChild mi m p = do
        case Map.lookup mi $ projectExternModules p of
            Just _ -> throwE $ DuplicateModule mi "Project.addExternModule" 
            Nothing -> return $ p{ projectExternModules = Map.insert mi m $ projectExternModules p }
    removeChild mi p = do
        case Map.lookup mi $ projectExternModules p of
            Nothing -> throwE undefined
            Just m -> return (m, p{ projectExternModules = Map.delete mi $ projectExternModules p })
    getChild mi p = do
        case Map.lookup mi $ projectExternModules p of
            Just m -> return m
            Nothing -> throwE undefined
    setChild mi mi' m' p = do
        case Map.lookup mi $ projectExternModules p of
            Just _ -> return $ p
                { projectExternModules
                    = Map.insert mi' m' 
                    $ Map.delete mi 
                    $ projectExternModules p 
                }
            Nothing -> throwE undefined

instance ParamEnvClass Module DeclarationInfo (WithBody Declaration) (SolutionError u) where
    addChild di d m = do
        case Map.lookup di $ moduleDeclarations m of
            Just _ -> throwE undefined
            Nothing -> return $ m{ moduleDeclarations = Map.insert di d $ moduleDeclarations m }
    removeChild di m = do
        case Map.lookup di $ moduleDeclarations m of
            Nothing -> throwE undefined
            Just d -> return (d, m{ moduleDeclarations = Map.delete di $ moduleDeclarations m })
    getChild di m = case Map.lookup di $ moduleDeclarations m of
        Just d -> return d
        Nothing -> throwE undefined
    setChild di di' d' m = case Map.lookup di $ moduleDeclarations m of
        Just _ -> return $ m
            { moduleDeclarations
                = Map.insert di' d' 
                $ Map.delete di 
                $ moduleDeclarations m
            }
        Nothing -> throwE undefined

instance ParamEnvClass Module ImportId (WithBody Import) (SolutionError u) where
    addChild ii i m = do
        case Map.lookup ii $ moduleImports m of
            Just _ -> throwE undefined
            Nothing -> return $ m{ moduleImports = Map.insert ii i $ moduleImports m }
    removeChild ii m = do
        case Map.lookup ii $ moduleImports m of
            Nothing -> throwE $ InvalidImportId (moduleInfo m) ii "Module.removeImport"
            Just i -> return (i, m{ moduleImports = Map.delete ii $ moduleImports m })
    getChild ii m = case Map.lookup ii $ moduleImports m of
        Just i -> return i
        nothing -> throwE undefined
    setChild ii ii' i' m = case Map.lookup ii $ moduleImports m of
        Just _ -> return $ m
            { moduleImports
                = Map.insert ii' i'
                $ Map.delete ii
                $ moduleImports m
            }
        Nothing -> throwE undefined

instance ParamEnvClass Module ExportId (WithBody Export) (SolutionError u) where
    addChild ei e m = do
        case moduleExports m of
            Just es -> case Map.lookup ei es of
                Just _ -> throwE undefined
                Nothing -> return $ m{ moduleExports = Just $ Map.insert ei e es }
            Nothing -> return $ m{ moduleExports = Just $ Map.insert ei e Map.empty }
    removeChild ei m = do
        case moduleExports m of
            Just es -> case Map.lookup ei es of
                Nothing -> throwE $ InvalidExportId (moduleInfo m) ei "Module.removeExport"
                Just e -> return (e, m{ moduleExports = Just $ Map.delete ei es })
            Nothing -> throwE $ InvalidExportId (moduleInfo m) ei "Module.removeExport"
    getChild ei m = case moduleExports m of
        Just es -> case Map.lookup ei es of
            Just e -> return e
            Nothing -> throwE undefined
        Nothing -> throwE undefined
    setChild ei ei' e' m = case moduleExports m of
        Just es -> case Map.lookup ei es of
            Just _ -> return $ m
                { moduleExports
                    = Just
                    $ Map.insert ei' e'
                    $ Map.delete ei
                    $ es
                }
            Nothing -> throwE undefined
        Nothing -> return $ m { moduleExports = Just $ Map.fromList [(ei',e')] }

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
    childEnv <- lift $ lift $ getChild childParam parentEnv
    result <- lift $ lift $ runReaderT f childEnv
    return result

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
    childEnv <- lift $ lift $ getChild childParam parentEnv
    (result,childEnv') <- lift $ lift $ runStateT f childEnv
    let childParam' = getParam childEnv'
    parentEnv' <- lift $ lift $ setChild childParam childParam' childEnv' parentEnv
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
    childEnv <- lift $ lift $ lift $ getChild childParam parentEnv
    (result,childEnv') <- lift $ lift $ runStateT f childEnv
    let childParam' = getParam childEnv'
    parentEnv' <- lift $ lift $ lift $ setChild childParam childParam' childEnv' parentEnv
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
    childEnv <- lift $ lift $ lift $ lift $ getChild childParam parentEnv
    (result,childEnv') <- lift $ lift $ runStateT f childEnv
    let childParam' = getParam childEnv'
    parentEnv' <- lift $ lift $ lift $ lift $ setChild childParam childParam' childEnv' parentEnv
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
    childEnv <- lift $ lift $ lift $ lift $ lift $ getChild childParam parentEnv
    (result,childEnv') <- lift $ lift $ runStateT f childEnv
    let childParam' = getParam childEnv'
    parentEnv' <- lift $ lift $ lift $ lift $ lift $ setChild childParam childParam' childEnv' parentEnv
    put parentEnv'
    return result

-- | A stateful operation over a value
type DescentChain1 a m u = StateT a (SolutionResult m u)

-- | A stateful operation over a value with an environment with a key
type DescentChain2 a b m u = StateT a (ReaderT b (SolutionResult m u))

-- | A stateful operation over a value with an environment with two keys
type DescentChain3 a b c m u = StateT a (ReaderT b (ReaderT c (SolutionResult m u)))

-- | A stateful operation over a value with an environment with three keys
type DescentChain4 a b c d m u = StateT a (ReaderT b (ReaderT c (ReaderT d (SolutionResult m u))))

-- | A stateful operation over a value with an environment with four keys
type DescentChain5 a b c d e m u = StateT a (ReaderT b (ReaderT c (ReaderT d (ReaderT e (SolutionResult m u)))))

-- | Run a stateful operation over a value
runDescent1 :: DescentChain1 a m u r -> a -> SolutionResult m u (r,a)
runDescent1 f a = runStateT f a 

-- | Run a stateful operation over a value with an environment with a key
runDescent2 :: Monad m => DescentChain2 a b m u r -> a -> b -> SolutionResult m u (r,a)
runDescent2 f a b = runReaderT (runStateT f a) b

-- | Run a stateful operation over a value with an environment with two keys
runDescent3 :: Monad m => DescentChain3 a b c m u r -> a -> b -> c -> SolutionResult m u (r,a)
runDescent3 f a b c = runReaderT (runReaderT (runStateT f a) b) c

-- | Run a stateful operation over a value with an environment with three keys
runDescent4 :: Monad m => DescentChain4 a b c d m u r -> a -> b -> c -> d -> SolutionResult m u (r,a)
runDescent4 f a b c d = runReaderT (runReaderT (runReaderT (runStateT f a) b) c) d

-- | Run a stateful operation over a value with an environment with four keys
runDescent5 :: Monad m => DescentChain5 a b c d e m u r -> a -> b -> c -> d -> e -> SolutionResult m u (r,a)
runDescent5 f a b c d e = runReaderT (runReaderT (runReaderT (runReaderT (runStateT f a) b) c) d) e

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

