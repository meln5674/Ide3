{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
module GuiClass 
    ( module GuiClass
    , module GuiClass.Types
    ) where

import Data.Text
import Data.Tree

import Control.Monad.Trans.Except

import Ide3.Types

import ErrorParser

import DeclarationPath

import GuiClass.Types

import SyntaxHighlighter2

import EnvironmentMonad

class Monad m => EditorBufferClass m where
    setEditorBufferText ::  Text -> m ()
    getEditorBufferText ::  Maybe CursorPosition -> Maybe CursorPosition -> m Text
    getEditorBufferCursor ::  m (CursorPosition, CursorPosition)
    selectEditorBufferText ::  CursorPosition -> CursorPosition -> m () 
    getEditorBufferPositionAtIndex ::  Int -> m CursorPosition
    getEditorBufferIndexAtPosition :: CursorPosition -> m Int
    insertTextAtEditorBufferPosition ::  CursorPosition -> Text -> m ()
    applySyntaxHighlighting :: [HighlightInst] -> m ()

class Monad m => EditorControlClass m where
    setEditorEnabled :: Bool -> m ()

class Monad m => BuildBufferClass m where
    setBuildBufferText :: Text -> m ()
    getBuildBufferText :: Maybe CursorPosition -> Maybe CursorPosition -> m Text
    getBuildBufferCursor :: m (CursorPosition, CursorPosition)
    selectBuildBufferText :: CursorPosition -> CursorPosition -> m ()

class Monad m => BuildControlClass m where
    setBuildEnabled :: Bool -> m ()
        
class Monad m => SearchBarClass m where
    getSearchBarText :: m Text

class Monad m => SolutionViewClass m where
    getElemAtSolutionTreePath :: TreePath -> m SolutionTreeElem
    getTreeAtSolutionTreePath :: TreePath -> m (Tree SolutionTreeElem)
    getForestAtSolutionTreePath :: TreePath -> m (Forest SolutionTreeElem)
    lookupAtSolutionTreePath :: TreePath -> m (Maybe (Tree SolutionTreeElem))
    setSolutionTree :: Forest SolutionTreeElem -> m ()
    updateSolutionTreePathNode :: TreePath -> (SolutionTreeElem -> SolutionTreeElem) -> m ()
    updateSolutionTreePathTree :: TreePath -> (Tree SolutionTreeElem -> Tree SolutionTreeElem) -> m ()
    insertSolutionTreePathNode :: TreePath -> Maybe Int -> SolutionTreeElem -> m ()
    insertSolutionTreePathTree :: TreePath -> Maybe Int -> Tree SolutionTreeElem -> m ()
    removeSolutionTreePathNode :: TreePath -> m ()
    

class (Monad m) => SolutionInitializerClass m where
    type ClassSolutionInitializerMonad m :: * -> *
    setupSolutionCreator :: m ()
    getSolutionCreatorArg 
        :: m (Either (SolutionError u) (ArgType (ClassSolutionInitializerMonad m)))
    finalizeSolutionCreator :: m ()

type SolutionInitializerClass' m m'
    = ( SolutionInitializerClass m
      -- , m' ~ ClassSolutionInitializerMonad m
      , InitializerMonad m'
      , Args (ArgType m')
      )



class (Monad m) => ProjectInitializerClass m where
    type ClassProjectInitializerMonad m :: * -> *
    setupProjectCreator :: Maybe (ProjectArgType (ClassProjectInitializerMonad m)) -> m ()
    getProjectCreatorArg 
        :: m (Either (SolutionError u) (ProjectArgType (ClassProjectInitializerMonad m)))
    finalizeProjectCreator :: m ()

type ProjectInitializerClass' m m'
    = ( ProjectInitializerClass m
      , m' ~ ClassProjectInitializerMonad m
      , ProjectInitializerMonad m'
      , Args (ProjectArgType m')
      )

class (Monad m) => SolutionEditorClass m where
    type ClassSolutionEditorMonad m :: * -> *
    setupSolutionEditor :: Maybe (SolutionEditArgType (ClassSolutionEditorMonad m)) -> m ()
    getSolutionEditorArg
        :: m (Either (SolutionError u) (SolutionEditArgType (ClassSolutionEditorMonad m)))
    finalizeSolutionEditor :: m ()

type SolutionEditorClass' m m'
    = ( SolutionEditorClass m
      , m' ~ ClassSolutionEditorMonad m
      , SolutionEditorMonad m'
      )

class Monad m => ErrorListClass m where
    clearErrorList :: m ()
    addErrorToList :: Error ItemPath Text -> m ()
    getErrorAtIndex :: Int -> m (Error ItemPath Text)

class Monad m => ErrorClass m where
    displayError :: Text -> m ()

setEditorBufferTextHighlighted :: EditorBufferClass m => Text -> m ()
setEditorBufferTextHighlighted text = do
    setEditorBufferText text
    result <- runExceptT $ getHighlights $ unpack text
    case result of
        Right hs -> applySyntaxHighlighting hs
        Left _ -> return ()

reapplySyntaxHighlighting :: EditorBufferClass m => m ()
reapplySyntaxHighlighting = do
    text <- getEditorBufferText Nothing Nothing
    result <- runExceptT $ getHighlights $ unpack text
    case result of
        Right hs -> applySyntaxHighlighting hs
        Left _ -> return ()
