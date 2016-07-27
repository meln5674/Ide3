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

import Ide3.Types hiding (getChild, getParam)

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

type DescentResult u m = ExceptT (SolutionError u) m

type DescentChain1 a u m = StateT a (DescentResult u m)
type DescentChain2 a b u m = StateT a (ReaderT b (DescentResult u m))
type DescentChain3 a b c u m = StateT a (ReaderT b (ReaderT c (DescentResult u m)))
type DescentChain4 a b c d u m = StateT a (ReaderT b (ReaderT c (ReaderT d (DescentResult u m))))
type DescentChain5 a b c d e u m = StateT a (ReaderT b (ReaderT c (ReaderT d (ReaderT e (DescentResult u m)))))

runDescent1 :: DescentChain1 a u m r -> a -> DescentResult u m (r,a)
runDescent1 f a = runStateT f a 

runDescent2 :: Monad m => DescentChain2 a b u m r -> a -> b -> DescentResult u m (r,a)
runDescent2 f a b = runReaderT (runStateT f a) b

runDescent3 :: Monad m => DescentChain3 a b c u m r -> a -> b -> c -> DescentResult u m (r,a)
runDescent3 f a b c = runReaderT (runReaderT (runStateT f a) b) c

runDescent4 :: Monad m => DescentChain4 a b c d u m r -> a -> b -> c -> d -> DescentResult u m (r,a)
runDescent4 f a b c d = runReaderT (runReaderT (runReaderT (runStateT f a) b) c) d

runDescent5 :: Monad m => DescentChain5 a b c d e u m r -> a -> b -> c -> d -> e -> DescentResult u m (r,a)
runDescent5 f a b c d e = runReaderT (runReaderT (runReaderT (runReaderT (runStateT f a) b) c) d) e


throw1 :: Monad m => SolutionError u -> DescentChain1 a u m r
throw1 = lift . throwE

throw2 :: Monad m => SolutionError u -> DescentChain2 a b u m r
throw2 = lift . lift . throwE

throw3 :: Monad m => SolutionError u -> DescentChain3 a b c u m r
throw3 = lift . lift . lift . throwE

throw4 :: Monad m => SolutionError u -> DescentChain4 a b c d u m r
throw4 = lift . lift . lift . lift . throwE
