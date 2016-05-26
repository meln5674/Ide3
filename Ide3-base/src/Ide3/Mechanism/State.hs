{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Ide3.Mechanism.State
Description : State monad instance of ProjectM
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides the ProjectStateT and ProjectState types.
These are simply StateT and State with a Project as their state type.

In addition, this provides an instance of ProjectM for any monad which
is a MonadState for Projects.
-}
module Ide3.Mechanism.State
    ( ProjectStateT
    , ProjectState
    , initialProject
    , runProjectStateT
    , runNewProjectStateT
--    , runProjectState
    , ProjectShellM (..)
    , ProjectStateM (..)
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict (StateT, runStateT, get, put)
import Control.Monad.Trans.Except
import Control.Monad.Identity

import Ide3.Monad
import Ide3.Types
import qualified Ide3.Project as Project

--import Ide3.HasA

type ProjectStateT = StateT Project
type ProjectState = ProjectStateT Identity

initialProject :: Project
initialProject = Project.new ProjectInfo

-- | Run a project state operation starting with an empty project
runProjectStateT :: Monad m => ProjectStateT m a -> Project -> m (a,Project)
runProjectStateT = runStateT

-- | Run a project state operation starting with an empty project
runNewProjectStateT :: Monad m => ProjectStateT m a -> m (a,Project)
runNewProjectStateT = flip runProjectStateT (Project.new ProjectInfo)

-- | Run a project state operation starting with an empty project
--runProjectState :: ProjectState a -> (a,Project)
--runProjectState f = runIdentity $ runProjectStateT f

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
    modifyProject :: Project -> a -> a
-}    

class Monad m => ProjectShellM m where
    load :: ProjectResult m u Project
    new :: ProjectInfo -> ProjectResult m u Project
    finalize :: Project -> ProjectResult m u ()

class Monad m => ProjectStateM m where
    getProject :: m Project
    putProject :: Project -> m ()

{-
instance {-# OVERLAPPABLE #-} (MonadState Project m) => ProjectStateM m where
    getProject = get
    putProject = put
-}

getsProject :: ProjectStateM m => (Project -> a) -> m a
getsProject f = f <$> getProject

modifyProject :: ProjectStateM m => (Project -> Project) -> m ()
modifyProject f = liftM f getProject >>= putProject

modifyProjectE :: ProjectStateM m => (Project -> Either (ProjectError u) Project) -> ProjectResult m u ()
modifyProjectE f = modifyProjectER $ \p -> do
    p' <- f p
    return (p',())

modifyProjectER :: ProjectStateM m => (Project -> Either (ProjectError u) (Project,a)) -> ProjectResult m u a
modifyProjectER f = do
    p <- lift getProject
    case f p of
        Right (p',r) -> do lift $ putProject p'
                           return r
        Left msg -> throwE msg

instance Monad m => ProjectStateM (ProjectStateT m) where
    getProject = get
    putProject = put

instance (ProjectShellM m, ProjectStateM m) => ProjectM m where
    load = Ide3.Mechanism.State.load >>= lift . putProject
    new i = Ide3.Mechanism.State.new i >>= lift . putProject
    finalize = lift getProject >>= Ide3.Mechanism.State.finalize

    editProjectInfo f = lift $ modifyProject $ \p -> p{projectInfo = f $ projectInfo p} 

    addModule m = modifyProjectE $ \p -> Project.addModule p m
    addExternModule m = modifyProjectE $ \p -> Project.addExternModule p m
    createModule i = modifyProjectE $ \p -> Project.createModule p i
    getModule i = ExceptT $ getsProject $ \p -> Project.getModule p i
    getExternModule i = ExceptT $ getsProject $ \p -> Project.getExternModule p i
    getModules = lift $ getsProject Project.allModules
    editModule i f = modifyProjectE $ \p -> Project.editModule p i f        
    removeModule i = modifyProjectE $ \p -> Project.removeModule p i

    addDeclaration i d = modifyProjectE $ \p -> Project.addDeclaration p i d
    getDeclaration i di = ExceptT $ getsProject $ \p -> getChild <$> Project.getDeclaration p (ModuleChild i di)
    getDeclarations i = ExceptT $ getsProject $ \p -> map getChild <$> Project.allDeclarationsIn p i
    editDeclaration i di f = modifyProjectE $ \p -> Project.editDeclaration p (ModuleChild i di) f
    removeDeclaration i di = modifyProjectE $ \p -> Project.removeDeclaration p (ModuleChild i di)

    addImport mi i = modifyProjectER $ \p -> Project.addImport p mi i
    getImport mi iid = ExceptT $ getsProject $ \p -> Project.getImport p mi iid
    removeImport mi i = modifyProjectE $ \p -> Project.removeImport p mi i
    getImports mi = ExceptT $ getsProject $ \p -> Project.getImports p mi
    
    addExport mi e = modifyProjectER $ \p -> Project.addExport p mi e
    getExport mi eid = ExceptT $ getsProject $ \p -> Project.getExport p mi eid
    removeExport mi e = modifyProjectE $ \p -> Project.removeExport p mi e
    exportAll mi = modifyProjectE $ \p -> Project.exportAll p mi
    exportNothing mi = modifyProjectE $ \p -> Project.exportNothing p mi
    getExports mi = ExceptT $ getsProject $ \p -> Project.getExports p mi
{-
--instance (ProjectShellM m, MonadState x m, HasA Project x) => ProjectM m where
instance (ProjectShellM m, HasA Project x) => ProjectM (StateT x m) where
    load = return ()
    new i = putA' $ Project.new i
    finalize = return ()
    editProjectInfo f = modifyA' $ \(Project i ms b) -> (Project (f i) ms b)
    addModule m = modifyEither $ \p -> Project.addModule p m
    createModule i = modifyEither $ \p -> Project.createModule p i
    getModule i = getsA' $ \p -> Project.getModule p i
    removeModule i = modifyEither $ \p -> Project.removeModule p i
    addDeclaration i d = modifyEither $ \p -> Project.addDeclaration p i d
    addImport mi i = modifyEitherR $ \p -> Project.addImport p mi i
    removeImport mi i = modifyEither $ \p -> Project.removeImport p mi i
    addExport mi e = modifyEitherR $ \p -> Project.addExport p mi e
    removeExport mi e = modifyEither $ \p -> Project.removeExport p mi e
    exportAll mi = modifyEither $ \p -> Project.exportAll p mi
    getModules = getsA' Project.allModules
-}

{-
class (Monad m) => ProjectStateM m where
    load :: m ()
    new :: ProjectInfo -> m ()
    finalize :: m ()
    getProject :: m Project
    putProject :: Project -> m ()
    modifyProject :: (Project -> Project) -> m ()
    getsProject :: (Project -> a) -> m a
    
    modifyProject f = getProject >>= \p -> putProject (f p)
    getsProject f = getProject >>= return . f




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
    modifyProject = State.modify
-}

{-
instance (ProjectStateM m) => ProjectM m where
    load = Ide3.Mechanism.State.load
    new = Ide3.Mechanism.State.new
    finalize = Ide3.Mechanism.State.finalize
    editProjectInfo f = modify $ \(Project i ms b) -> (Project (f i) ms b)
    addModule m = modifyEither $ \p -> Project.addModule p m
    createModule i = modifyEither $ \p -> Project.createModule p i
    getModule i = gets $ \p -> Project.getModule p i
    removeModule i = modifyEither $ \p -> Project.removeModule p i
    addDeclaration i d = modifyEither $ \p -> Project.addDeclaration p i d
    addImport mi i = modifyEitherR $ \p -> Project.addImport p mi i
    removeImport mi i = modifyEither $ \p -> Project.removeImport p mi i
    addExport mi e = modifyEitherR $ \p -> Project.addExport p mi e
    removeExport mi e = modifyEither $ \p -> Project.removeExport p mi e
    exportAll mi = modifyEither $ \p -> Project.exportAll p mi
    getModules = gets Project.allModules    

-}