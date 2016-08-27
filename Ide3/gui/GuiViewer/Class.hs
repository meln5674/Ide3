module GuiViewer.Class where

import Ide3.Types
import SearchMode
import DeclarationPath


class Monad m => GuiViewerClass m where
    setSearchMode :: SearchMode -> m ()
    getSearchMode :: m SearchMode
    openDeclaration :: DeclarationPath -> m ()
    closeDeclaration :: DeclarationPath -> m ()
    getOpenDeclarations :: m [DeclarationPath]
    declarationIsOpen :: DeclarationPath -> m Bool
    openDeclarationInHistory :: DeclarationPath -> String -> m ()
    replaceHistoryPath :: DeclarationPath -> m ()
    replaceHistoryText :: String -> m ()
    navigateHistoryBack :: m (Maybe (DeclarationPath, String))
    navigateHistoryForward :: m (Maybe (DeclarationPath, String))
