{-# LANGUAGE MultiParamTypeClasses #-}
module GuiClass 
    ( module GuiClass
    , module GuiClass.Types
    ) where

import Data.Tree

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types

import ErrorParser.Types

import DeclarationPath
import SearchMode

import GuiClass.Types

import SyntaxHighlighter2



class Monad m => EditorBufferClass m where
    setEditorBufferText ::  String -> m ()
    getEditorBufferText ::  Maybe CursorPosition -> Maybe CursorPosition -> m String
    getEditorBufferCursor ::  m (CursorPosition, CursorPosition)
    selectEditorBufferText ::  CursorPosition -> CursorPosition -> m () 
    getEditorBufferPositionAtIndex ::  Int -> m CursorPosition
    getEditorBufferIndexAtPosition :: CursorPosition -> m Int
    insertTextAtEditorBufferPosition ::  CursorPosition -> String -> m ()
    applySyntaxHighlighting :: [HighlightInst] -> m ()

class Monad m => BuildBufferClass m where
    setBuildBufferText :: String -> m ()
    getBuildBufferText :: Maybe CursorPosition -> Maybe CursorPosition -> m String
    getBuildBufferCursor :: m (CursorPosition, CursorPosition)
    selectBuildBufferText :: CursorPosition -> CursorPosition -> m ()
    
        
class Monad m => SearchBarClass m where
    getSearchBarText :: m String

class Monad m => SolutionViewClass m where
    getElemAtSolutionPath :: TreePath -> m SolutionTreeElem
    getTreeAtSolutionPath :: TreePath -> m (Tree SolutionTreeElem)
    getForestAtSolutionPath :: TreePath -> m (Forest SolutionTreeElem)
    setSolutionTree :: [Tree SolutionTreeElem] -> m ()
    updateSolutionTreeNode :: TreePath -> (SolutionTreeElem -> SolutionTreeElem) -> m ()


class Monad m => ErrorListClass m where
    clearErrorList :: m ()
    addErrorToList :: Error ItemPath -> m ()

class Monad m => ErrorClass m where
    displayError :: String -> m ()

setEditorBufferTextHighlighted :: EditorBufferClass m => String -> m ()
setEditorBufferTextHighlighted text = do
    setEditorBufferText text
    result <- runExceptT $ getHighlights text
    case result of
        Right hs -> applySyntaxHighlighting hs
        Left _ -> return ()

reapplySyntaxHighlighting :: EditorBufferClass m => m ()
reapplySyntaxHighlighting = do
    text <- getEditorBufferText Nothing Nothing
    result <- runExceptT $ getHighlights text
    case result of
        Right hs -> applySyntaxHighlighting hs
        Left _ -> return ()
