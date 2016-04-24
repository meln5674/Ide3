{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad

import System.Console.Haskeline

import Ide3.Mechanism.State (ProjectStateT)

import qualified CmdParser as Cmd
import CmdParser
import Command
import Viewer
import ReadOnlyFilesystemProject

repl :: (MonadException m, ViewerMonad m) => InputT (CommandT UserError (ViewerStateT m)) Bool
repl = do
    input <- getInputLine ">"
    case input of
        Nothing -> return True
        Just input -> do
            response <- lift $ execCommand input ()
            outputStrLn response
            continue <- lift $ getExitFlag
            return continue

runMain :: (MonadException m, ViewerMonad m) => InputT (CommandT UserError (ViewerStateT m)) ()
runMain = do
    continue <- repl
    when continue runMain

settings :: (MonadException m, ViewerMonad m) => Settings (CommandT UserError (ViewerStateT m))
settings = Settings{complete=cmdCompletion, historyFile=Nothing, autoAddHistory=True}

commandList :: (MonadIO m, ViewerMonad m) => [Command u (ViewerStateT m)]
commandList =
    [ helpCmd
    , openCmd
    , saveCmd
    , saveAsCmd
    , modulesCmd
    , moduleCmd
    , declarationsCmd
    , importsCmd
    , importedCmd
    , exportsCmd
    , exportedCmd
    , visibleCmd
    , catCmd
    , treeCmd
    , searchCmd
    , quitCmd
    ]


runWith :: (MonadException (t (ProjectStateT IO)), ViewerMonad (t (ProjectStateT IO)))
        => (forall b . t (ProjectStateT IO) b -> fsp -> ProjectStateT IO (b, fsp))
        -> fsp 
        -> IO ()
runWith runFspT unopened = void $ 
    runViewerState runFspT unopened $
        flip runCommandT commandList $ 
            runInputT settings $ do
                outputStrLn "Haskell project viewer"
                outputStrLn "Type \"help\" for commands"
                runMain


main :: IO ()
main = runWith runReadOnlyFilesystemProjectT Unopened
