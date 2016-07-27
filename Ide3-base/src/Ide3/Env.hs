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



class EnvParamClass env param | env -> param where
    getParam :: env -> param

class ParamEnvClass parentEnv childParam childEnv e where
    getChild :: Monad m => childParam -> parentEnv -> ExceptT e m childEnv
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


{-
type SolutionEnvT = StateT Solution
type ProjectEnvT = StateT Project
type ModuleEnvT = StateT Module
type DeclarationEnvT = StateT Declaration

type ProjectParamEnvT = ReaderT ProjectInfo
type ModuleParamEnvT = ReaderT ModuleInfo
type DeclarationParamEnvT = ReaderT DeclarationInfo
type ParamEnvT p = ReaderT p

type ParamT p = ReaderT p


type Sol u = SolutionEnvT (DescentResult u)
type SolProj u = SolutionEnvT (ProjectParamEnvT (DescentResult u))
type SolProjMod u = SolutionEnvT (ProjectParamEnvT (ModuleParamEnvT (DescentResult u)))
type SolProjModDecl u = SolutionEnvT (ProjectParamEnvT (ModuleParamEnvT (DeclarationParamEnvT (DescentResult u))))

type Proj u = ProjectEnvT (DescentResult u)
type ProjMod u = ProjectEnvT (ModuleParamEnvT (DescentResult u))
type ProjModDecl u = ProjectEnvT (ModuleParamEnvT (DeclarationParamEnvT (DescentResult u)))

type Mod u = ModuleEnvT (DescentResult u)
type ModDecl u = ModuleEnvT (DeclarationParamEnvT (DescentResult u))

type Decl u = DeclarationEnvT (DescentResult u)

type SolParam p u = SolutionEnvT (DescentResult u)
type SolProjParam p u = SolutionEnvT (ProjectParamEnvT (DescentResult u))
type SolProjModParam p u = SolutionEnvT (ProjectParamEnvT (ModuleParamEnvT (DescentResult u)))
type SolProjModDeclParam p u = SolutionEnvT (ProjectParamEnvT (ModuleParamEnvT (DeclarationParamEnvT (DescentResult u))))

type ProjParam p u = ProjectEnvT (DescentResult u)
type ProjModParam p u = ProjectEnvT (ModuleParamEnvT (DescentResult u))
type ProjModDeclParam p u = ProjectEnvT (ModuleParamEnvT (DeclarationParamEnvT (DescentResult u)))

type ModParam p u = ModuleEnvT (DescentResult u)
type ModDeclParam p u = ModuleEnvT (DeclarationParamEnvT (DescentResult u))

type DeclParam p u = DeclarationEnvT (DescentResult u)
-}

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
