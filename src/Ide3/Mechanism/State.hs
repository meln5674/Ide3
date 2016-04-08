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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Ide3.Mechanism.State
    ( ProjectStateT
    , ProjectState
    , runProjectStateT
    , runProjectState
    ) where

import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.State.Class
import Control.Monad.Identity

import Ide3.Monad        
import Ide3.Types
import qualified Ide3.Project as Project

type ProjectStateT m = StateT Project m
type ProjectState = ProjectStateT Identity

runProjectStateT :: Monad m => ProjectStateT m a -> m (a,Project)
runProjectStateT f = runStateT f (Project.new ProjectInfo)

runProjectState :: ProjectState a -> (a,Project)
runProjectState f = runIdentity $ runProjectStateT f

modifyEither :: MonadState s m => (s -> Either l s) -> m (Either l ())
modifyEither f = modifyEitherR (\s -> (\r -> (r,())) <$> f s)

modifyEitherR :: MonadState s m => (s -> Either l (s,a)) -> m (Either l a)
modifyEitherR f = do
    s <- get
    let r = f s
    case r of
        Right (s',x) -> do put s'
                           return $ Right x
        Left l -> return $ Left l

instance (Monad m, MonadState Project m) => ProjectM m where
    load = error "Cannot load a test project"
    new i = put $ Project.new i
    finalize = error "Cannot finalize a test project"
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
