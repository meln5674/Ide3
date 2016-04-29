module Editor (Editor, runEditor, nanoEditor, EditorResult (..)) where

import Control.Exception.Base hiding (catch)

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Catch

import System.Process
import System.IO
import System.IO.Temp

import Ide3.Monad
import Ide3.Types

data EditorResult
    = EditConfirmed String
    | DeleteConfirmed
    | EditCanceled

newtype Editor m u = MkEditor { runEditorInternal :: String -> ProjectResult m u EditorResult }

runEditor :: Editor m u -> String -> ProjectResult m u EditorResult
runEditor = runEditorInternal

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
