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
{-# LANGUAGE RankNTypes #-}
module Editor
    ( Editor
    , runEditor
    , nanoEditor
    , noEditor
    , EditorResult (..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Exception.Base hiding (catch)

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Catch

import System.Process
import System.IO
import System.IO.Temp

import Ide3.Types

-- | The result from running an editor
data EditorResult
    -- | The text should be replaced with contained string
    = EditConfirmed Text
    -- | The text should be deleted
    | DeleteConfirmed
    -- | The text should be left alone
    | EditCanceled

-- | Abstract type for editors
newtype Editor m = MkEditor { runEditorInternal :: forall u . Text -> SolutionResult u m EditorResult }

-- | Run an editor on a given string
runEditor :: Editor m -> Text -> SolutionResult u m EditorResult
runEditor = runEditorInternal

-- | An editor which represents no editing capability and will always result in an error
noEditor :: Monad m => Editor m
noEditor = MkEditor $ \_ -> throwE $ Unsupported "No editor specified"

-- | An editor which invokes the nano program
nanoEditor :: (MonadIO m, MonadMask m) => Editor m
nanoEditor = MkEditor $ \toEdit -> ExceptT $ flip catch handleException $
    withSystemTempFile ".hs" $ \path h -> liftIO $ do
        hClose h
        T.writeFile path toEdit
        callCommand $ "nano " ++ path
        contents <- liftIO $ T.readFile path
        return $ Right $ EditConfirmed contents
  where
    handleException e = return $ Left $ InvalidOperation (show (e :: IOException)) ""
