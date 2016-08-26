module GuiViewer.Class where

import Ide3.Types
import SearchMode 


class Monad m => GuiViewerClass m where
    setSearchMode :: SearchMode -> m ()
    getSearchMode :: m SearchMode
    openDeclaration :: DeclarationInfo -> m ()
    closeDeclaration :: DeclarationInfo -> m ()
    getOpenDeclarations :: m [DeclarationInfo]
    declarationIsOpen :: DeclarationInfo -> m Bool
