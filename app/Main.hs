module Main where

import Data.List
import Data.Maybe

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Control.Monad

import System.Environment
import System.IO (hFlush, stdout)

import System.Console.Haskeline
import System.Console.Haskeline.Completion

import Ide3.Monad

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Module as Module

import Ide3.Types (item, items, body, Module, ModuleInfo(..), Symbol(..), getChild, ProjectError, DeclarationInfo (..), ProjectError (..))

import Ide3.Mechanism.State ( runProjectStateT )
import Ide3.Mechanism

import qualified CmdParser as Cmd
import CmdParser
import Digest
import Viewer

--readCmd :: MonadIO m => InputT m (Maybe Cmd)
readCmd = do
    input <- getInputLine ">"
    case input of
        Nothing -> return Nothing
        Just input -> case Cmd.parse input of
            Right cmd -> return $ Just cmd
            Left err -> outputStrLn err >> return Nothing

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
        , "quit: exit the program"
        ]

type UserError = ()

printOnError :: ProjectResult ViewerStateM UserError String -> ViewerStateM String
printOnError f = liftM (either show id) $ runExceptT f

type Output a = Either a String

asShow = Left
asShows = map asShow
asString = Right
asStrings = map asString

output :: Show a => Output a -> String
output (Left a) = show a
output (Right s) = s

withSelectedModule :: Show a => (Module -> ProjectResult ViewerStateM UserError [Output a]) -> ViewerStateM String
withSelectedModule f = printOnError $ do
    name <- lift $ gets currentModule
    case name of
        Nothing -> throwE $ InvalidOperation "No module currently selected" ""
        Just name -> do
            mod <- getModule (ModuleInfo (Symbol name))
            xs <- f mod
            return $ intercalate "\n" $ map output xs

doHelp :: ViewerStateM String
doHelp = return printHelp

doOpen :: FilePath -> ViewerStateM String
doOpen path = printOnError $ do
    lift . lift $ put (ToOpen path)
    lift $ modify $ \s -> s{currentModule=Nothing}
    load
    lift . lift $ put (Opened path)
    return "Loaded"

doModules :: ViewerStateM String
doModules = printOnError $ do
    names <- getModules
    return $ intercalate "\n" $ map show names

doModule :: String -> ViewerStateM String
doModule name = printOnError $ do
    getModule (ModuleInfo (Symbol name))
    lift $ modify $ \s -> s{currentModule=Just name}
    return ""

doDeclarations :: ViewerStateM String
doDeclarations = withSelectedModule 
    $ return 
    . asShows
    . map (Declaration.info) 
    . items 
    . Module.getDeclarations

doImports :: ViewerStateM String
doImports = withSelectedModule 
    $ return
    . asShows
    . items
    . Module.getImports

doImported :: ViewerStateM String
doImported = withSelectedModule
    $ liftM asShows 
    . Module.importedSymbols
    
doExports :: ViewerStateM String
doExports = withSelectedModule
    $ return
    . asShows
    . items
    . Module.getExports

doExported :: ViewerStateM String
doExported = withSelectedModule
    $   (return 
    .   asShows
    .   map getChild)
    <=< Module.exportedSymbols 

doVisible :: ViewerStateM String
doVisible = withSelectedModule $ liftM asShows . Module.internalSymbols

doCat :: String -> ViewerStateM String
doCat sym = withSelectedModule $ \mod -> ExceptT $ return $ do
    decl <- Module.getDeclaration mod (DeclarationInfo (Symbol sym))
    let strings :: [Output Bool]
        strings = asStrings . lines . body . getChild $ decl
    return strings

doQuit :: ViewerStateM String
doQuit = return ""

repl :: InputT ViewerStateM Bool
repl = do
    input <- readCmd
    output <- case input of
        Nothing -> return $ Just ""
        Just Quit -> return Nothing
        Just cmd -> liftM Just $ lift $ case cmd of
            Help -> doHelp
            Open path -> doOpen path
            Modules -> doModules
            Module name -> doModule name
            Declarations -> doDeclarations
            Imports -> doImports
            Imported -> doImported
            Exports -> doExports
            Exported -> doExported
            Visible -> doVisible
            Cat sym -> doCat sym
    case output of
        Just output -> outputStrLn output >> return True
        Nothing -> return False

runMain :: InputT ViewerStateM ()
runMain = do
    continue <- repl
    when continue runMain

settings :: Settings ViewerStateM
settings = Settings{complete=cmdCompletion, historyFile=Nothing, autoAddHistory=True}


isPrefixOfCommand l 
    = mapMaybe
        (\x -> if (l `isPrefixOf` x && l /= x) then Just x else Nothing)
      cmdList

isCommandPrefixOf l'
    = mapMaybe
        (\x -> if x == l then Just (l,r) else Nothing)
      cmdList
  where
    (l,r) = case words l' of
        [] -> ("","")
        (x:xs) -> (x,unwords xs)
    
for xs f = map f xs


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
isCommandAllowed "quit" = return True

cmdPrefixCompletion :: String -> Maybe [Completion]
cmdPrefixCompletion l = 
  case isPrefixOfCommand l of
        [] -> Nothing
        xs -> Just $ for xs $ \x -> 
                Completion{ replacement=drop (length l) x
                          , display = x
                          , isFinished = null $ isPrefixOfCommand x
                          }

moduleNameCompletion :: String -> ViewerStateM (Maybe [Completion])
moduleNameCompletion s = do
    p <- hasOpenedProject
    case p of
        False -> return Nothing
        True -> do
            r <- runExceptT $ do
                mods <- getModules
                let modNames = catMaybes $ for mods  $ \m -> case m of
                        (ModuleInfo (Symbol s)) -> Just s
                        _ -> Nothing
                let matchingNames = filter (s `isPrefixOf`) modNames
                return $ Just $ for matchingNames $ \n ->
                    Completion{ replacement = drop (length s) n
                              , display = n
                              , isFinished = not $ any (\n' -> n `isPrefixOf` n' && n /= n') matchingNames
                              }
            return $ case r of
                Right x -> x
                Left _ -> Nothing
declarationNameCompletion :: String -> ViewerStateM (Maybe [Completion])
declarationNameCompletion s = do
    n <- gets currentModule
    case n of
        Nothing -> return Nothing
        Just n -> do
            r <- runExceptT $ do
                m <- getModule (ModuleInfo (Symbol n))
                let infos = map (Declaration.info . item) $ Module.getDeclarations m
                let syms = for infos $ \(DeclarationInfo (Symbol sym)) -> sym
                let matchingSyms = filter (s `isPrefixOf`) syms
                return $ Just $ for matchingSyms $ \n -> 
                    Completion{ replacement = drop (length s) n
                              , display = n
                              , isFinished = not $ any (\n' -> n `isPrefixOf` n' && n /= n') matchingSyms
                              }    
            return $ case r of
                Right x -> x
                Left _ -> Nothing

cmdArgCompletion :: String -> String -> ViewerStateM (Maybe [Completion])
cmdArgCompletion cmd arg = do
    p <- isCommandAllowed cmd
    case p of
        False -> return Nothing
        True -> case cmd of
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
            "quit" -> return (Just [])


cmdCompletion :: (String,String) -> ViewerStateM (String, [Completion])
cmdCompletion (l',r) = do
    case cmdPrefixCompletion l of
        Just cs -> do
            cs' <- filterM (isCommandAllowed . (l ++) . replacement) cs
            return (l',cs')
        Nothing -> do
            let cs = isCommandPrefixOf l
            cs' <- filterM (isCommandAllowed . fst) cs
            cs'' <- forM cs' $ \(cmd,arg) -> cmdArgCompletion cmd arg
            let cs''' = concat $ catMaybes cs''
            return (l',cs''')
  where
    l = reverse l'
      
main :: IO ()
main = do
    runViewerState $ do
        runInputT settings $ do
            outputStrLn "Haskell project viewer"
            outputStrLn "Type \"help\" for commands"
            runMain
    return ()
