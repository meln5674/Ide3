module Main where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad

import System.Environment
import System.IO (hFlush, stdout)

import Ide3.Monad

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Module as Module

import Ide3.Types (items, body, Module, ModuleInfo(..), Symbol(..), getChild, ProjectError, DeclarationInfo (..))

import Ide3.Mechanism.State ( runProjectStateT )
import Ide3.Mechanism

import qualified CmdParser as Cmd
import CmdParser
import Digest
import Viewer

readCmd :: IO (Maybe Cmd)
readCmd = do
    input <- getLine
    case Cmd.parse input of
        Right cmd -> return $ Just cmd
        Left err -> putStrLn err >> return Nothing

printHelp = do
    mapM_ putStrLn $
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
        , "quit: exit the program"
        ]

printOnError f = do
    runExceptT $ catchE f $ liftIO . putStrLn
    return ()

type Output a = Either a String

asShow = Left
asShows = map asShow
asString = Right
asStrings = map asString

output :: Show a => Output a -> IO ()
output (Left a) = print a
output (Right s) = putStrLn s

withSelectedModule :: Show a => (Module -> ExceptT ProjectError ViewerStateM [Output a]) -> ViewerStateM ()
withSelectedModule f = printOnError $ do
    name <- lift $ gets currentModule
    case name of
        Nothing -> throwE "No module currently selected"
        Just name -> do
            mod <- getModule (ModuleInfo (Symbol name))
            xs <- f mod
            liftIO $ mapM_ output xs

doHelp :: ViewerStateM ()
doHelp = liftIO printHelp

doOpen :: FilePath -> ViewerStateM ()
doOpen path = printOnError $ do
    lift . lift $ put (ToOpen path)
    lift $ modify $ \s -> s{currentModule=Nothing}
    load

doModules = printOnError $ do
    names <- getModules
    liftIO $ mapM_ print names

doModule :: String -> ViewerStateM ()
doModule name = printOnError $ do
    getModule (ModuleInfo (Symbol name))
    lift $ modify $ \s -> s{currentModule=Just name}

doDeclarations :: ViewerStateM ()
doDeclarations = withSelectedModule 
    $ return 
    . asShows
    . map (Declaration.info) 
    . items 
    . Module.getDeclarations

doImports :: ViewerStateM ()
doImports = withSelectedModule 
    $ return
    . asShows
    . items
    . Module.getImports

doImported :: ViewerStateM ()
doImported = withSelectedModule
    $ liftM asShows 
    . Module.importedSymbols
    
doExports :: ViewerStateM ()
doExports = withSelectedModule
    $ return
    . asShows
    . items
    . Module.getExports

doExported :: ViewerStateM ()
doExported = withSelectedModule
    $   (return 
    .   asShows
    .   map getChild)
    <=< Module.exportedSymbols 

doVisible :: ViewerStateM ()
doVisible = withSelectedModule $ liftM asShows . Module.internalSymbols

doCat :: String -> ViewerStateM ()
doCat sym = withSelectedModule $ \mod -> ExceptT $ return $ do
    decl <- Module.getDeclaration mod (DeclarationInfo (Symbol sym))
    let strings :: [Output Bool]
        strings = asStrings . lines . body . getChild $ decl
    return strings

doQuit :: ViewerStateM ()
doQuit = return ()



repl = do
    input <- liftIO $ readCmd
    case input of
        Just Help -> doHelp >> return True
        Just (Open path) -> doOpen path >> return True
        Just Modules -> doModules >> return True
        Just (Module name) -> doModule name >> return True
        Just Declarations -> doDeclarations >> return True
        Just Imports -> doImports >> return True
        Just Imported -> doImported >> return True
        Just Exports -> doExports >> return True
        Just Exported -> doExported >> return True
        Just (Cat sym) -> doCat sym >> return True
        Just Quit -> return False
        Nothing -> return True

runMain = do
    liftIO $ putStr ">" >> hFlush stdout
    continue <- repl
    when continue runMain
      
main :: IO ()
main = do
    putStrLn "Haskell project viewer"
    putStrLn "Type \"help\" for commands"
    runViewerState runMain
    return ()
