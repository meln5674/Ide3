{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module GuiViewer.Internal where

import Data.Text (Text)

import Data.List (delete)

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.Lift
import Ide3.NewMonad.Instances.Lift.TH


import Viewer

import PseudoState

import SearchMode 

import History (History)
import qualified History

import GuiViewer.Class

import DeclarationPath

data GuiViewerState
    = GuiViewerState
    { openDeclarations :: [SolutionPath]
    , searchMode :: SearchMode
    , declarationHistory :: History (SolutionPath, Text)
    }

emptyGuiViewer :: GuiViewerState
emptyGuiViewer = GuiViewerState [] Find History.empty

newtype GuiViewerT m a = GuiViewer { runGuiViewer :: StateT GuiViewerState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

deriving instance (MonadMask m) => MonadMask (GuiViewerT m)
deriving instance (MonadCatch m) => MonadCatch (GuiViewerT m)
deriving instance (MonadThrow m) => MonadThrow (GuiViewerT m)

instance Monad m => GuiViewerClass (GuiViewerT m) where
    setSearchMode mode = GuiViewer $ modify $ \st -> st { searchMode = mode }
    getSearchMode = GuiViewer $ gets searchMode
    openDeclaration di = do
        isDuplicate <- declarationIsOpen di
        when (not isDuplicate) $ do
            GuiViewer $ modify $ \s -> s
                { openDeclarations = di : openDeclarations s 
                , declarationHistory = History.singleton (di,"")
                }
    closeDeclaration di = GuiViewer $ do
        modify $ \s -> s
            { openDeclarations = delete di $ openDeclarations s 
            }
    getOpenDeclarations = GuiViewer $ gets openDeclarations
    declarationIsOpen di = GuiViewer $ gets $ (di `elem`) . openDeclarations
    openDeclarationInHistory di text = GuiViewer $ do
        modify $ \s -> s
            { openDeclarations = case () of
                ()
                    | di `elem` openDeclarations s -> openDeclarations s
                    | otherwise -> di : openDeclarations s
            , declarationHistory = History.insertBack (di,text) $ History.abandonFuture $ declarationHistory s
            }
    replaceHistoryPath di' = GuiViewer $ do
        history <- gets declarationHistory
        case History.present history of
            Nothing -> return ()
            Just (_,text) -> 
                case History.replace (di',text) history of
                    Just history' -> do
                        modify $ \s -> s{ declarationHistory = history' }
                    Nothing -> return ()
    replaceHistoryText text' = GuiViewer $ do
        history <- gets declarationHistory
        case History.present history of
            Nothing -> return ()
            Just (di,_) -> 
                case History.replace (di,text') history of
                    Just history' -> do
                        modify $ \s -> s{ declarationHistory = history' }
                    Nothing -> return ()    
    updateHistoryPath p p' = GuiViewer $ do
        history <- gets declarationHistory
        open <- gets openDeclarations
        let updatePath x
                | x == p = p'
                | otherwise = x
            updatePair (x,t) = (updatePath x, t)
            history' = fmap updatePair history
            open' = fmap updatePath open
        modify $ \st -> st { declarationHistory = history' 
                           , openDeclarations = open'
                           }
    navigateHistoryBack = GuiViewer $ do
        history <- gets declarationHistory
        case History.back history of
            Just history' -> case History.present history' of
                Just di -> do
                    modify $ \s -> s{ declarationHistory = history' }
                    return $ Just di
                Nothing -> return Nothing
            Nothing -> return Nothing
    navigateHistoryForward = GuiViewer $ do
        history <- gets declarationHistory
        case History.forward history of
            Just history' -> case History.present history' of
                Just di -> do
                    modify $ \s -> s{ declarationHistory = history' }
                    return $ Just di
                Nothing -> return Nothing
            Nothing -> return Nothing

instance PseudoStateT GuiViewerT GuiViewerState where
    runPseudoStateT f s = runStateT (runGuiViewer f) s

instance MonadBounce GuiViewerT where
    bounce = ExceptT . lift . runExceptT

