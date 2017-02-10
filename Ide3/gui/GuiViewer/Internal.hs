{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module GuiViewer.Internal where

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Set as S

import Data.Text (Text)

import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.Utils
import Ide3.NewMonad.Instances.Lift()

import Viewer()

import PseudoState

import SearchMode 

import History (History)
import qualified History

import GuiViewer.Class

import DeclarationPath

data GuiViewerState
    = GuiViewerState
    { openDeclarations :: Map SolutionPath Text
    , searchMode :: SearchMode
    , declarationHistory :: History SolutionPath
    }

emptyGuiViewer :: GuiViewerState
emptyGuiViewer = GuiViewerState M.empty Find History.empty

newtype GuiViewerT m a = GuiViewer { runGuiViewer :: StateT GuiViewerState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

deriving instance (MonadMask m) => MonadMask (GuiViewerT m)
deriving instance (MonadCatch m) => MonadCatch (GuiViewerT m)
deriving instance (MonadThrow m) => MonadThrow (GuiViewerT m)

instance Monad m => GuiViewerClass (GuiViewerT m) where
    setSearchMode mode = GuiViewer $ modify $ \st -> st { searchMode = mode }
    getSearchMode = GuiViewer $ gets searchMode
    
    openDeclaration di text = GuiViewer $ do
        let addDecl s
                | di `M.member` openDeclarations s = s
                | otherwise = s { openDeclarations = M.insert di text $ openDeclarations s }
            addToHistory s = s { declarationHistory = History.insertBack di $ History.abandonFuture $ declarationHistory s }
        modify $ addToHistory . addDecl
    getOpenDeclarations = GuiViewer $ gets $ S.fromList . M.keys . openDeclarations
    getOpenDeclaration di = GuiViewer $ gets $ M.lookup di . openDeclarations
    
    getCurrentHistory = GuiViewer $ do
        history <- gets declarationHistory
        decls <- gets openDeclarations
        return $ do
            di <- History.present history
            text <- M.lookup di decls
            return (di, text)
        
    replaceHistoryPath di' = GuiViewer $ do
        history <- gets declarationHistory
        case History.present history of
            Nothing -> return ()
            Just di -> 
                case History.replace di' history of
                    Just history' -> 
                        modify $ \s -> s{ declarationHistory = history' }
                    Nothing -> return ()
    replaceHistoryText text' = GuiViewer $ do
        history <- gets declarationHistory
        case History.present history of
            Nothing -> return ()
            Just di -> modify $ \s -> s { openDeclarations = M.insert di text' $ openDeclarations s }
    updateHistoryPath p p' = GuiViewer $ do
        history <- gets declarationHistory
        open <- gets openDeclarations
        let updatePath x
                | x == p = p'
                | otherwise = x
            history' = fmap updatePath history
            open' = M.delete p $ M.insert p' (open M.! p) open
        modify $ \st -> st { declarationHistory = history' 
                           , openDeclarations = open'
                           }
    navigateHistoryBack = GuiViewer $ do
        history <- gets declarationHistory
        case History.back history of
            Just history' -> case History.present history' of
                Just di -> do
                    modify $ \s -> s{ declarationHistory = history' }
                    text <- gets $ (M.! di) . openDeclarations
                    return $ Just (di, text)
                Nothing -> return Nothing
            Nothing -> return Nothing
    navigateHistoryForward = GuiViewer $ do
        history <- gets declarationHistory
        case History.forward history of
            Just history' -> case History.present history' of
                Just di -> do
                    modify $ \s -> s{ declarationHistory = history' }
                    text <- gets $ (M.! di) . openDeclarations
                    return $ Just (di, text)
                Nothing -> return Nothing
            Nothing -> return Nothing

instance PseudoStateT GuiViewerT GuiViewerState where
    runPseudoStateT = runStateT . runGuiViewer

instance MonadBounce GuiViewerT where
    bounce = ExceptT . lift . runExceptT


