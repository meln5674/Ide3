module GuiViewer.Class where

import Ide3.Types
import SearchMode
import DeclarationPath


class Monad m => GuiViewerClass m where
    setSearchMode :: SearchMode -> m ()
    getSearchMode :: m SearchMode
    openDeclaration :: SolutionPath -> m ()
    closeDeclaration :: SolutionPath -> m ()
    getOpenDeclarations :: m [SolutionPath]
    declarationIsOpen :: SolutionPath -> m Bool
    openDeclarationInHistory :: SolutionPath -> String -> m ()
    replaceHistoryPath :: SolutionPath -> m ()
    replaceHistoryText :: String -> m ()
    navigateHistoryBack :: m (Maybe (SolutionPath, String))
    navigateHistoryForward :: m (Maybe (SolutionPath, String))
