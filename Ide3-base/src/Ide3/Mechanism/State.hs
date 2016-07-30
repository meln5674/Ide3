{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : Ide3.Mechanism.State
Description : State monad instance of SolutionM
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides the SolutionStateT and SolutionState types.
These are simply StateT and State with a Solution as their state type.

In addition, this provides an instance of SolutionM for any monad which
is a MonadState for Solutions.
-}
module Ide3.Mechanism.State
    ( initialSolution
    , runSolutionStateT
    , runNewSolutionStateT
--    , runSolutionState
--    , mkStatefulSolution
    , module Ide3.Mechanism.State.Types
    , module Ide3.Mechanism.State.Helpers
    ) where

import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict (StateT, runStateT, get, put)

import Ide3.Monad
import Ide3.Types hiding ( getChild )
import qualified Ide3.Env.Solution as Solution

import Ide3.Mechanism.State.Types
import Ide3.Mechanism.State.Helpers

import Ide3.Env

--import Ide3.HasA

-- | The initial in-memory solution
initialSolution :: Solution
initialSolution = Solution.new (SolutionInfo "")

-- | Run a Solution state operation starting with an empty Solution
runSolutionStateT :: Monad m => SolutionStateT m a -> Solution -> m (a,Solution)
runSolutionStateT = runStateT . runSolutionStateTInternal

-- | Run a Solution state operation starting with an empty Solution
runNewSolutionStateT :: Monad m => SolutionStateT m a -> m (a,Solution)
runNewSolutionStateT = flip runSolutionStateT initialSolution

-- | Run a project state operation starting with an empty project
--runProjectState :: ProjectState a -> (a,Project)
--runProjectState f = runIdentity $ runSolutionStateT f

{-
-- | Same as 'modifyEitherR', but the transformation produces no additional result
modifyEither :: (HasA s' s, MonadState s m) => (s' -> Either l s') -> m (Either l ())
modifyEither f = modifyEitherR (\s -> (\r -> (r,())) <$> f s)

-- | Apply a possibly failing transformation to a state
modifyEitherR :: (HasA s' s, MonadState s m) => (s' -> Either l (s',a)) -> m (Either l a)
modifyEitherR f = do
    s <- getA'
    let r = f s
    case r of
        Right (s',x) -> do putA' s'
                           return $ Right x
        Left l -> return $ Left l
-}
{-
class HasProject a where
    getProject :: a -> Project
    modifySolution :: Project -> a -> a
-}    


instance (SolutionStateM m) => SolutionStateM (StateT s m) where
    getSolution = lift getSolution
    putSolution = lift . putSolution

{-
instance ProjectShellM m => ProjectShellM (StateT s m) where
    new x = ExceptT $ lift $ runExceptT $ Ide3.Mechanism.State.Types.new x
    load = ExceptT $ lift $ runExceptT Ide3.Mechanism.State.Types.load
    finalize x = ExceptT $ lift $ runExceptT $ Ide3.Mechanism.State.Types.finalize x
-}

instance Monad m => SolutionStateM (SolutionStateT m) where
    getSolution = SolutionStateT get
    putSolution = SolutionStateT . put


instance MonadTrans StatefulSolution where
    lift = MkStatefulSolution

--mkStatefulSolution :: (SolutionShellM m, SolutionStateM m) => m a -> StatefulSolution m a
--mkStatefulSolution = MkStatefulSolution

--liftStatefulSolution :: m a -> StatefulSolution m a
--liftStatefulSolution 

instance (SolutionShellM m, SolutionStateM m) => SolutionM (StatefulSolution m) where
    load = Ide3.Mechanism.State.Types.load >>= lift . putSolution
    new i = Ide3.Mechanism.State.Types.new i >>= lift . putSolution
    finalize = lift getSolution >>= Ide3.Mechanism.State.Types.finalize
    
    editSolutionInfo f = lift $ modifySolution $ \s -> s{ solutionInfo = f $ solutionInfo s }
    
    addProject a = modifySolutionEnv $ \s -> runDescent2 Solution.addProject s a
    removeProject a = modifySolutionEnv $ \s -> runDescent2 Solution.removeProject s a
    getProjects = modifySolutionEnv $ \s -> runDescent1 Solution.getProjects s
    editProjectInfo a b = modifySolutionEnv $ \s -> runDescent3 Solution.editProjectInfo s a b

    addModule a b = modifySolutionEnv $ \s -> runDescent3 Solution.addModule s a b
    addExternModule a b = modifySolutionEnv $ \s -> runDescent3 Solution.addExternModule s a b
    createModule a b = modifySolutionEnv $ \s -> runDescent3 Solution.createModule s a b
    getModule a b = modifySolutionEnv $ \s -> runDescent3 Solution.getModule s a b
    getExternModule a b = modifySolutionEnv $ \s -> runDescent3 Solution.getExternModule s a b
    getModules a = modifySolutionEnv $ \s -> runDescent2 Solution.allModules s a
    editModule a b c = modifySolutionEnv $ \s -> runDescent4 Solution.editModule s a b c
    removeModule a b = modifySolutionEnv $ \s -> runDescent3 Solution.removeModule s a b

    addDeclaration a b c = modifySolutionEnv $ \s -> runDescent4 Solution.addDeclaration s a b c
    getDeclaration a b c  = modifySolutionEnv $ \s -> runDescent4 Solution.getDeclaration s a b c
    getDeclarations a b = modifySolutionEnv $ \s -> runDescent3 Solution.getDeclarations s a b
    editDeclaration a b c d = modifySolutionEnv $ \s -> runDescent5 Solution.editDeclaration s a b c d
    removeDeclaration a b c = modifySolutionEnv $ \s -> runDescent4 Solution.removeDeclaration s a b c

    addImport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.addImport s a b c
    getImport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.getImport s a b c
    removeImport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.removeImport s a b c
    getImports a b = modifySolutionEnv $ \s -> runDescent3 Solution.getImports s a b
    
    addExport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.addExport s a b c
    getExport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.getExport s a b c
    removeExport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.removeExport s a b c
    exportAll a b = modifySolutionEnv $ \s -> runDescent3 Solution.exportAll s a b
    exportNothing a b = modifySolutionEnv $ \s -> runDescent3 Solution.exportNothing s a b
    getExports a b = modifySolutionEnv $ \s -> runDescent3 Solution.getExports s a b

    addPragma a b c = modifySolutionEnv $ \s -> runDescent4 Solution.addPragma s a b c
    removePragma a b c = modifySolutionEnv $ \s -> runDescent4 Solution.removePragma s a b c
    getPragmas a b = modifySolutionEnv $ \s -> runDescent3 Solution.getPragmas s a b

deriving instance (MonadMask m) => MonadMask (SolutionStateT m)
deriving instance (MonadMask m) => MonadMask (StatefulSolution m)
deriving instance (MonadCatch m) => MonadCatch (SolutionStateT m)
deriving instance (MonadCatch m) => MonadCatch (StatefulSolution m)
deriving instance (MonadThrow m) => MonadThrow (SolutionStateT m)
deriving instance (MonadThrow m) => MonadThrow (StatefulSolution m)

{-
--instance (ProjectShellM m, MonadState x m, HasA Project x) => SolutionM m where
instance (ProjectShellM m, HasA Project x) => SolutionM (StateT x m) where
    load = return ()
    new i = putA' $ Solution.new i
    finalize = return ()
    editProjectInfo f = modifyA' $ \(Project i ms b) -> (Project (f i) ms b)
    addModule m = modifyEither $ \s -> Solution.addModule p m
    createModule i = modifyEither $ \s -> Solution.createModule p i
    getModule i = getsA' $ \s -> Solution.getModule p i
    removeModule i = modifyEither $ \s -> Solution.removeModule p i
    addDeclaration i d = modifyEither $ \s -> Solution.addDeclaration p i d
    addImport mi i = modifyEitherR $ \s -> Solution.addImport p mi i
    removeImport mi i = modifyEither $ \s -> Solution.removeImport p mi i
    addExport mi e = modifyEitherR $ \s -> Solution.addExport p mi e
    removeExport mi e = modifyEither $ \s -> Solution.removeExport p mi e
    exportAll mi = modifyEither $ \s -> Solution.exportAll p mi
    getModules = getsA' Solution.allModules
-}

{-
class (Monad m) => ProjectStateM m where
    load :: m ()
    new :: ProjectInfo -> m ()
    finalize :: m ()
    getProject :: m Project
    putProject :: Project -> m ()
    modifySolution :: (Project -> Project) -> m ()
    getsSolution :: (Project -> a) -> m a
    
    modifySolution f = getProject >>= \s -> putProject (f p)
    getsSolution f = getProject >>= return . f




instance (Monad m, ProjectStateM m) => MonadState Project m where
    get = getProject
    put = putProject
-}
{-
class (MonadState Project m) => ProjectStateM m where
    load :: m ()
    new :: ProjectInfo -> m ()
    finalize :: m ()
-}
--editProjectInfoS :: (ProjectInfo -> ProjectInfo) ->

{-
instance (Monad m, MonadState Project m) => ProjectStateM m where
    load = M.load
    new = M.new
    finalize = M.finalize
    getProject = State.get
    putProject = State.put
    modifySolution = State.modify
-}

{-
instance (ProjectStateM m) => SolutionM m where
    load = Ide3.Mechanism.State.load
    new = Ide3.Mechanism.State.new
    finalize = Ide3.Mechanism.State.finalize
    editProjectInfo f = modify $ \(Project i ms b) -> (Project (f i) ms b)
    addModule m = modifyEither $ \s -> Solution.addModule p m
    createModule i = modifyEither $ \s -> Solution.createModule p i
    getModule i = gets $ \s -> Solution.getModule p i
    removeModule i = modifyEither $ \s -> Solution.removeModule p i
    addDeclaration i d = modifyEither $ \s -> Solution.addDeclaration p i d
    addImport mi i = modifyEitherR $ \s -> Solution.addImport p mi i
    removeImport mi i = modifyEither $ \s -> Solution.removeImport p mi i
    addExport mi e = modifyEitherR $ \s -> Solution.addExport p mi e
    removeExport mi e = modifyEither $ \s -> Solution.removeExport p mi e
    exportAll mi = modifyEither $ \s -> Solution.exportAll p mi
    getModules = gets Solution.allModules    

-}
