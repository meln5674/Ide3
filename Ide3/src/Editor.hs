{-|
Module      : Editor
Description : Editors for the demo solution
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module defines the Editor type which represent an action that invokes a
text editor on a given peice of text
-}
module Editor
    ( Editor
    , runEditor
    , nanoEditor
    , noEditor
    , EditorResult (..)
    ) where

import Control.Exception.Base hiding (catch)

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Catch

import System.Process
import System.IO
import System.IO.Temp

import Ide3.Monad
import Ide3.Types

-- | The result from running an editor
data EditorResult
    -- | The text should be replaced with contained string
    = EditConfirmed String
    -- | The text should be deleted
    | DeleteConfirmed
    -- | The text should be left alone
    | EditCanceled

-- | Abstract type for editors
newtype Editor m u = MkEditor { runEditorInternal :: String -> SolutionResult m u EditorResult }

-- | Run an editor on a given string
runEditor :: Editor m u -> String -> SolutionResult m u EditorResult
runEditor = runEditorInternal

-- | An editor which represents no editing capability and will always result in an error
noEditor :: Monad m => Editor m u
noEditor = MkEditor $ \_ -> throwE $ Unsupported "No editor specified"

-- | An editor which invokes the nano program
nanoEditor :: (MonadIO m, MonadMask m) => Editor m u
nanoEditor = MkEditor $ \toEdit -> ExceptT $ flip catch handleException $
    withSystemTempFile ".hs" $ \path h -> liftIO $ do
        hClose h
        writeFile path toEdit
        callCommand $ "nano " ++ path
        contents <- liftIO $ readFile path
        return $ Right $ EditConfirmed contents
  where
    handleException e = return $ Left $ InvalidOperation (show (e :: IOException)) ""
