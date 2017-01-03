module GuiViewer.Class where

import Data.Text

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
    openDeclarationInHistory :: SolutionPath -> Text -> m ()
    replaceHistoryPath :: SolutionPath -> m ()
    replaceHistoryText :: Text -> m ()
    updateHistoryPath :: SolutionPath -> SolutionPath -> m ()
    navigateHistoryBack :: m (Maybe (SolutionPath, Text))
    navigateHistoryForward :: m (Maybe (SolutionPath, Text))
