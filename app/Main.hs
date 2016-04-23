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

{-
readCmd :: InputT ViewerStateM (Maybe Cmd)
readCmd = do
    input <- getInputLine ">"
    case input of
        Nothing -> return Nothing
        Just line -> case Cmd.parse line of
            Right cmd -> return $ Just cmd
            Left err -> outputStrLn err >> return Nothing


-}
{-
printHelp :: String
printHelp = intercalate "\n"
        [ "Commands: "
        , ""
        , "help: show this message"
        , "open PATH: open a project rooted at PATH"
        , "modules: show a list of modules in the current project"
        , "module MODULE: set MODULE as the current module"
        , "declarations: list the declarations in the current module"
        , "imports: list the imports in the current module"
        , "imported: list the symbols imported by the current module"
        , "exports: list the exports in the current module"
        , "exported: list the symbols exported by the current module"
        , "visible: list the symbols visible at the top level of the current module"
        , "cat SYMBOL: show the body of the declaration of SYMBOL"
        , "tree: Display a tree of the current project's modules and declarations"
        , "quit: exit the program"
        ]
-}


        
{-
A
|- x
|- y
|
+-- A.B
|   |- a
|   |- b
|   |
|   +-- A.B.D
|   |   |
|   |   +-- A.B.D.E
|   |       |
|   |       +-- A.B.D.E.G
|   |
|   +-- A.B.F
|
+-- A.C
    |
    +-- A.C.D

E
|
+-- E.F
    |- 1
    |- 2


Steps:
PREFIX +-- MODULENAME
[
    {PREFIX |- DECLARATIONNAME} DECLARATIONS
    |
]
[
    {
        PREFIX |
        SUBMODULE, PREFIX+= |
    } SUBMODULES
    |
]
PREFIX



-}



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

{-
isPrefixOfCommand :: String -> [String]
isPrefixOfCommand l 
    = mapMaybe
        (\x -> if l `isPrefixOf` x && l /= x then Just x else Nothing)
      cmdList

isCommandPrefixOf :: String -> [(String, String)]
isCommandPrefixOf l'
    = mapMaybe
        (\x -> if x == l then Just (l,r) else Nothing)
      cmdList
  where
    (l,r) = case words l' of
        [] -> ("","")
        (x:xs) -> (x,unwords xs)
    


isCommandAllowed :: String -> ViewerStateM Bool
isCommandAllowed "help" = return True
isCommandAllowed "open" = return True
isCommandAllowed "module" = hasOpenedProject
isCommandAllowed "modules" = hasOpenedProject
isCommandAllowed "declarations" = hasCurrentModule
isCommandAllowed "imports" = hasCurrentModule
isCommandAllowed "imported" = hasCurrentModule
isCommandAllowed "exports" = hasCurrentModule
isCommandAllowed "exported" = hasCurrentModule
isCommandAllowed "visible" = hasCurrentModule
isCommandAllowed "cat" = hasCurrentModule
isCommandAllowed "tree" = hasOpenedProject
isCommandAllowed "quit" = return True
isCommandAllowed _ = return False

cmdPrefixCompletion :: String -> Maybe [Completion]
cmdPrefixCompletion l = 
  case isPrefixOfCommand l of
        [] -> Nothing
        xs -> Just $ for xs $ \x -> 
                Completion{ replacement=drop (length l) x
                          , display = x
                          , isFinished = null $ isPrefixOfCommand x
                          }
-}

{-
cmdArgCompletion :: String -> String -> ViewerStateM (Maybe [Completion])
cmdArgCompletion cmd arg = do
    p <- isCommandAllowed cmd
    if not p
        then return Nothing
        else case cmd of
            "help" -> return (Just [])
            "open" -> liftM Just $ listFiles arg
            "module" -> moduleNameCompletion arg
            "modules" -> return (Just [])
            "declarations" -> return (Just [])
            "imports" -> return (Just [])
            "imported" -> return (Just [])
            "exports" -> return (Just [])
            "exported" -> return (Just [])
            "visible" -> return (Just [])
            "cat" -> declarationNameCompletion arg
            "tree" -> return (Just [])
            "quit" -> return (Just [])
            x -> error $ "Internal Error: Invalid command " ++ x ++ " was parsed"

cmdCompletion :: (String,String) -> ViewerStateM (String, [Completion])
cmdCompletion (l',_) =
    case cmdPrefixCompletion l of
        Just cs -> do
            cs' <- filterM (isCommandAllowed . (l ++) . replacement) cs
            return (l',cs')
        Nothing -> do
            let cs = isCommandPrefixOf l
            cs' <- filterM (isCommandAllowed . fst) cs
            cs'' <- forM cs' $ uncurry cmdArgCompletion
            let cs''' = concat $ catMaybes cs''
            return (l',cs''')
  where
    l = reverse l'
-}

commandList =
    [ helpCmd
    , openCmd
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
