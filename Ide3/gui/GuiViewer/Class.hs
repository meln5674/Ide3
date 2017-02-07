module GuiViewer.Class where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text

import Control.Monad

import SearchMode
import DeclarationPath

-- | Monads which can perform gui specific operations
class Monad m => GuiViewerClass m where
    -- | Set the search mode
    setSearchMode :: SearchMode -> m ()
    -- | Retreive the search mode
    getSearchMode :: m SearchMode
    
    -- | Open a declaration at the given path with the given text.
    -- Implementations should not overwrite an existing open declaration with
    -- the same path.
    openDeclaration :: SolutionPath -> Text -> m ()
    -- | Retreive the paths of all open declarations
    getOpenDeclarations :: m (Set SolutionPath)
    -- | Test if a declaration is open
    getOpenDeclaration :: SolutionPath -> m (Maybe Text)

    -- | Retrieve the currently open item in the history
    getCurrentHistory :: m (Maybe (SolutionPath, Text))
    
    -- | Replace the path of the current item in the history
    replaceHistoryPath :: SolutionPath -> m ()
    -- | Replace the text of the current item in the history
    replaceHistoryText :: Text -> m ()
    
    -- | Find all instances of the a path in the history and replace them
    -- with a new path
    updateHistoryPath :: SolutionPath -> SolutionPath -> m ()
    
    -- Go to the next item in history and return the path and text if it exists
    navigateHistoryBack :: m (Maybe (SolutionPath, Text))
    -- Go to the previous item in history and return the path and text if it exists
    navigateHistoryForward :: m (Maybe (SolutionPath, Text))
    
