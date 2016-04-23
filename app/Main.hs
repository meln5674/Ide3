{-# LANGUAGE LambdaCase #-}
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



import qualified CmdParser as Cmd
import CmdParser
import Command
import Viewer

repl :: InputT (CommandT UserError ViewerStateM) Bool
repl = do
    input <- getInputLine ">"
    case input of
        Nothing -> return True
        Just input -> do
            response <- lift $ execCommand input ()
            outputStrLn response
            continue <- lift $ getExitFlag
            return continue

runMain :: InputT (CommandT UserError ViewerStateM) ()
runMain = do
    continue <- repl
    when continue runMain

settings :: Settings (CommandT UserError ViewerStateM)
settings = Settings{complete=cmdCompletion, historyFile=Nothing, autoAddHistory=True}

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

main :: IO ()
main = void $ 
    runViewerState $
        flip runCommandT commandList $ 
            runInputT settings $ do
                outputStrLn "Haskell project viewer"
                outputStrLn "Type \"help\" for commands"
                runMain
