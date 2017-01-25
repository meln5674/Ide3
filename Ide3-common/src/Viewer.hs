{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
module Viewer
    ( module Viewer
    , module Viewer.Internal
    ) where

import System.Directory

import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

import Control.Monad.Catch

import Ide3.Types
import Ide3.NewMonad
import Ide3.NewMonad.Instances.Lift.TH
import Ide3.Utils
    
import Viewer.Internal



[betterderiving| lift; ; ViewerMonad; ViewerStateT; prepareBuild=splice2 |]

[betterderiving|
splice2;
;
  PersistenceClass
, SolutionClass
, ProjectModuleClass
, ModuleDeclarationClass
, ModuleImportClass
, ModuleExportClass
, ModulePragmaClass
, ModuleFileClass
, ModuleLocationClass
, ProjectExternModuleClass
, ExternModuleExportClass;
ViewerStateT;
|]


{-
prepareToken :: ( MonadIO m
                , ViewerMonad m
                )
             => FilePath 
             -> m (Maybe (ViewerPersistToken m))
prepareToken path = do
    isFile <- liftIO $ doesFileExist path
    isDir <- liftIO $ doesDirectoryExist path
    case (isFile, isDir) of
        (True, _) -> do
            Just <$> prepareFilePathToken path
        (_,True) -> do
            Just <$> prepareDirectoryPathToken path
        (_,_) -> return Nothing 
-}

-- | Open a solution at a given path
openSolution :: ( ViewerStateClass m
                , PersistenceClass m 
                , ViewerPersistToken m ~ PersistToken m
                )
            => FilePath 
            -> SolutionResult u m ()
openSolution path = do
    tok <- lift $ preparePathToken path
    lift $ setNoCurrentDecl
    load tok
--        Nothing -> throwE $ InvalidOperation (path ++ " does not exist") ""
{-
    isFile <- liftIO $ doesFileExist path
    isDir <- liftIO $ doesDirectoryExist path
    case (isFile, isDir) of
        (True, _) -> do
            tok <- lift $ prepareFilePathToken path
            lift $ setNoCurrentDecl
            load tok
        (_,True) -> do
            tok <- lift $ prepareDirectoryPathToken path
            lift $ setNoCurrentDecl
            load tok
        (_,_) -> throwE $ InvalidOperation (path ++ " does not exist") ""
-}
-- | Save the current solution, optionally with a new path to save to
saveSolution :: ( ViewerMonad m
                , PersistenceClass m
                , ViewerStateClass m
                , ViewerPersistToken m ~ PersistToken m
                ) 
            => Maybe FilePath
            -> SolutionResult u m ()
saveSolution Nothing = do
    pred <- lift hasOpenedSolution
    if pred
        then finalize Nothing
        else throwE $ InvalidOperation "No solution is currently open" ""
saveSolution (Just path) = do
    cond <- lift hasOpenedSolution
    tok <- lift $ preparePathToken path
    if cond
        then finalize $ Just tok
        else throwE $ InvalidOperation "No solution is currently open" ""

instance ViewerMonad m => ViewerStateClass (ViewerStateT m) where
    setCurrentProject pji = ViewerStateT $ put $ Viewer (Just pji) Nothing Nothing
    setCurrentModule pji mi = ViewerStateT $ put $ Viewer (Just pji) (Just mi) Nothing
    setCurrentDecl pji mi di = ViewerStateT $ put $ Viewer (Just pji) (Just mi) (Just di)
    getCurrentProject = ViewerStateT $ gets currentProject
    getCurrentModule = ViewerStateT $ do
        maybePji <- gets currentProject
        maybeMi <- gets currentModule
        case (maybePji, maybeMi) of
            (Just pji, Just mi) -> return $ Just (pji,mi)
            _ -> return Nothing
    getCurrentDeclaration = ViewerStateT $ do
        maybePji <- gets currentProject
        maybeMi <- gets currentModule
        maybeDi <- gets currentDecl
        case (maybePji, maybeMi, maybeDi) of
            (Just pji, Just mi, Just di) -> return $ Just (pji,mi,di)
            _ -> return Nothing
    setNoCurrentDecl = ViewerStateT $ put $ Viewer Nothing Nothing Nothing
    

deriving instance (MonadMask m) => MonadMask (ViewerStateT m)
deriving instance (MonadCatch m) => MonadCatch (ViewerStateT m)
deriving instance (MonadThrow m) => MonadThrow (ViewerStateT m)

