{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Command where

import Data.Maybe
import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Applicative (empty)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State.Strict

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P

import System.Console.Haskeline

import Ide3.Monad

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Module as Module

import Ide3.Types
    ( item
    , items
    , body
    , Module
    , ModuleInfo(..)
    , Symbol(..)
    , getChild
    , DeclarationInfo (..)
    , ProjectError (..)
    , ModuleChild (..)
    , Qualify (..)
    )

import Ide3.Mechanism.State()

import CmdParser
import Viewer
import ViewerMonad
import Editor

for :: [a] -> (a -> b) -> [b]
for xs f = map f xs


--newtype CommandT u m a = CommandT (ReaderT [Command u m] (StateT Bool m) a)
type CommandT u m = ReaderT [Command u m] (StateT Bool m)

runCommandT :: Monad m => CommandT u m a -> [Command u m] -> m a
runCommandT f = flip evalStateT True . runReaderT f

runCommandT' :: Monad m => CommandT u m a -> Bool -> [Command u m] -> m (a,Bool)
runCommandT' f flag = flip runStateT flag . runReaderT f

getCommands :: Monad m => CommandT u m [Command u m]
getCommands = ask

getExitFlag :: Monad m => CommandT u m Bool
getExitFlag = lift get

setExitFlag :: Monad m => CommandT u m ()
setExitFlag = lift $ put False

liftCmd :: Monad m => m a -> CommandT u m a
liftCmd = lift . lift

liftMCmd :: Monad m => (a1 -> r) -> CommandT u m a1 -> CommandT u m r
liftMCmd f = mapReaderT $ liftM f

{-
CommandT :: Monad m => ([Command u m] -> m a) -> CommandT u m a
CommandT f = CommandT $ do
    cmds <- ask
    r <- lift $ f cmds
    return (r,True)
-}
{-
instance Functor m => Functor (CommandT u m) where
    fmap f (CommandT m) = CommandT $ fmap f m

instance Monad m => Applicative (CommandT u m) where
    pure = return
    (<*>) = ap

bindCommandT :: forall a b u m . Monad m => CommandT u m a -> (a -> CommandT u m b) -> CommandT u m b
bindCommandT x f = CommandT $ ReaderT $ \cmds -> StateT $ \s -> do
    (z,s') <- runCommandT' x s cmds
    let g :: CommandT u m b -> m (b, Bool)
        g = \y -> runCommandT' y s' cmds
        h :: a -> m (b, Bool)
        h =  g . f
    h z

instance Monad m => Monad (CommandT u m) where
    return x = CommandT $ return x
    (>>=) = bindCommandT


instance MonadTrans (CommandT u) where
    lift f = CommandT $ lift $ lift f


instance MonadIO m => MonadIO (CommandT u m) where
    liftIO f = CommandT $ liftIO f

--mapCommandT f m = CommandT $ f . runCommandT m


instance MonadException m => MonadException (CommandT u m) where
{-    controlIO f = CommandT $ ReaderT $ \cmds -> controlIO $ \(RunIO run) -> let
        run' :: _
        run' = RunIO (fmap (CommandT . const) . run . runCommandT cmds)
        thing :: _
        thing = do
            x <- fmap (runCommandT cmds) $ f run' :: _
            return (x,True)
        in thing
-}
    controlIO f = do
-}        


type UserError = ()

printOnError :: ViewerMonad m => ProjectResult (ViewerStateT m) UserError String -> ViewerStateT m String
printOnError f = liftM (either show id) $ runExceptT f

newtype Output a = MkOutput (Either a String)

asShow :: Show a => a -> Output a
asShow = MkOutput . Left
asShows :: Show a => [a] -> [Output a]
asShows = map asShow
asString :: String -> Output a
asString = MkOutput . Right
asStrings :: [String] -> [Output a]
asStrings = map asString

instance Show a => Show (Output a) where
    show (MkOutput (Left a)) = show a
    show (MkOutput (Right a)) = a

withSelectedModule :: (Show a, ViewerMonad m) 
                   => (Module -> ProjectResult (ViewerStateT m) UserError [Output a]) 
                   -> ViewerStateT m String
withSelectedModule f = printOnError $ do
    maybeName <- lift $ gets currentModule
    case maybeName of
        Nothing -> throwE $ InvalidOperation "No module currently selected" ""
        Just name -> do
            module_ <- getModule (ModuleInfo (Symbol name))
            xs <- f module_
            return $ intercalate "\n" $ map show xs

doHelp :: ViewerMonad m => CommandT u m String
doHelp = printHelp

doOpen :: (MonadIO m, ViewerMonad m) => FilePath -> ViewerStateT m String
doOpen path = printOnError $ do
    openProject path
    return "Loaded"

doSave :: (MonadIO m, ViewerMonad m) => ViewerStateT m String
doSave = printOnError $ do
    saveProject Nothing
    return "Saved"

doSaveAs :: (MonadIO m, ViewerMonad m) => FilePath -> ViewerStateT m String
doSaveAs p = printOnError $ do
    saveProject $ Just p
    return "Saved"

doModules :: ViewerMonad m => ViewerStateT m String
doModules = printOnError $ do
    names <- getModules
    return $ intercalate "\n" $ map show names

doModule :: ViewerMonad m => String -> ViewerStateT m String
doModule name = printOnError $ do
    _ <- getModule (ModuleInfo (Symbol name))
    lift $ modify $ \s -> s{currentModule=Just name}
    return ""

doDeclarations :: ViewerMonad m => ViewerStateT m String
doDeclarations = withSelectedModule 
    $ return 
    . asShows
    . map Declaration.info
    . items 
    . Module.getDeclarations

doImports :: ViewerMonad m => ViewerStateT m String
doImports = withSelectedModule 
    $ return
    . asShows
    . items
    . Module.getImports

doImported :: ViewerMonad m => ViewerStateT m String
doImported = withSelectedModule
    $ liftM asShows 
    . Module.importedSymbols
    
doExports :: ViewerMonad m => ViewerStateT m String
doExports = withSelectedModule
    $ return
    . asShows
    . items
    . Module.getExports

doExported :: ViewerMonad m => ViewerStateT m String
doExported = withSelectedModule
    $   (return 
    .   asShows
    .   map getChild)
    <=< Module.exportedSymbols 

doVisible :: ViewerMonad m => ViewerStateT m String
doVisible = withSelectedModule $ liftM asShows . Module.internalSymbols

doCat :: ViewerMonad m => String -> ViewerStateT m String
doCat sym = withSelectedModule $ \module_ -> ExceptT $ return $ do
    decl <- Module.getDeclaration module_ (DeclarationInfo (Symbol sym))
    let strings :: [Output Bool]
        strings = asStrings . lines . body . getChild $ decl
    return strings

doEdit :: (MonadIO m, ViewerMonad m) => (forall u . Editor m u) -> String -> ViewerStateT m String
doEdit editor sym = withSelectedModule $ \module_ -> do
    let declInfo = DeclarationInfo (Symbol sym)
    let moduleInfo = Module.info module_
    decl <- ExceptT $ return $ Module.getDeclaration module_ declInfo
    let declBody = body $ getChild decl
    newDeclBody <- ExceptT $ lift $ runExceptT $ runEditor editor declBody
    case newDeclBody of
        EditConfirmed newBody -> do 
            editDeclaration moduleInfo declInfo $ \_ -> Declaration.parseAndCombine newBody Nothing
            return [asShow "Edit completed"]
        DeleteConfirmed -> do
            removeDeclaration moduleInfo declInfo
            return [asShow "Delete completed"]
        EditCanceled -> return [asShow "Edit canceled"]
    

doTree :: ViewerMonad m => ViewerStateT m String
doTree = printOnError $ do
    trees <- makeTree
    return $ intercalate "\n \n" $ map formatTree trees
    --return $ show trees

doSearch :: ViewerMonad m => String -> ViewerStateT m String
doSearch sym = printOnError $ do
    modules <- getModules
    let matchingDeclsFrom info = do
            decls <- lift $ getDeclarations info
            let matches = filter (\(DeclarationInfo (Symbol sym')) -> sym `isInfixOf` sym') decls
                taggedMatches = map (ModuleChild info) matches
            tell taggedMatches
    matchingDecls <- execWriterT $ forM modules matchingDeclsFrom
    return $ intercalate "\n" $ map (show . qual) matchingDecls

doQuit :: ViewerMonad m => CommandT u (ViewerStateT m) String
doQuit = do
    setExitFlag
    return "Exiting..."

partitionBy :: Ord k => (a -> k) -> [a] -> Map k [a]
partitionBy f = foldl (\m x -> Map.alter (\case { Nothing -> Just [x]; Just ys -> Just (x:ys) }) (f x) m) Map.empty

data ModuleTree
    = OrgNode ModuleInfo [ModuleTree]
    | ModuleNode ModuleInfo [DeclarationInfo] [ModuleTree]
    deriving Show

makeTreeSkeleton :: [ModuleInfo] -> [ModuleTree]
makeTreeSkeleton = go ""
  where
    go knownRoot modInfos = for partitions processPartition
      where
        processPartition (rootInfo,subModuleNames) = makeNode rootInfo newRoot toProcess
          where
            rootPresent = rootInfo `elem` subModuleNames
            rootString = case rootInfo of
                ModuleInfo (Symbol s) -> s
                UnamedModule (Just p) -> p
            newRoot = rootString ++ "."
            toProcess = if rootPresent
                then delete rootInfo subModuleNames
                else subModuleNames
            makeNode x y z = if rootPresent
                then ModuleNode x [] $ go y z
                else OrgNode x $ go y z
        partitions = Map.toList $ partitionBy getRootName modInfos
        getRootName (ModuleInfo (Symbol s)) = ModuleInfo $ Symbol $ preRoot ++ postRoot
          where
            preRoot = take (length knownRoot) s
            postRoot = takeWhile ('.' /= ) $ drop (length knownRoot) s
        getRootName x = x


{-
Take a list of module names
Find all root module names
Collect module names into lists tagged with the root
For each list:
    If the root is present:
        remove the root
        Repeat the process on the sub-list, not considering the root as part of the name
        Collect the result into a ModuleNode with empty declarations
    If the root is not present:
        Repeat the process on the sub-list, not considering the root as part of the name
        Collect the result into an OrgNode

-}

fillTree :: ProjectM m => ModuleTree -> ProjectResult m u ModuleTree
fillTree (OrgNode i ts) = do
    ts' <- mapM fillTree ts
    return $ OrgNode i ts'
fillTree (ModuleNode i _ ts) = do
    ds <- getDeclarations i
    ts' <- mapM fillTree ts
    return $ ModuleNode i ds ts'

makeTree :: ProjectM m => ProjectResult m u [ModuleTree]
makeTree = do
    modules <- getModules
    let emptyTree = makeTreeSkeleton modules
    mapM fillTree emptyTree
    
formatTree :: ModuleTree -> String
formatTree = intercalate "\n" . go []
  where
    go prefixFlags tree = lines
      where
        buildPrefix [] = ""
        buildPrefix (True:xs)  = buildPrefix xs ++ "|   "
        buildPrefix (False:xs) = buildPrefix xs ++ "    "
        prefix = buildPrefix prefixFlags
        headPrefix = buildPrefix (drop 1 prefixFlags)
        decls = case tree of 
            OrgNode{} -> []
            ModuleNode _ ds _ -> ds
        subModules = case tree of
            OrgNode _ ms -> ms
            ModuleNode _ _ ms -> ms
        moduleInfo = case tree of
            OrgNode n _ -> n
            ModuleNode n _ _ -> n
        moduleName = case moduleInfo of
            ModuleInfo (Symbol n) -> n
            UnamedModule (Just p) -> p
        firstLine = case prefixFlags of
            [] -> moduleName
            [_] -> "+-- " ++ moduleName
            _ -> headPrefix ++ "+-- " ++ moduleName
        declLines = case decls of
            [] -> []
            ds -> map ((prefix ++) . ("|- " ++) . makeLine) ds
              where
                makeLine (DeclarationInfo (Symbol s)) = s
        subModuleLines = case subModules of
            [] -> []
            ms -> firstModuleLines ++ lastModuleLines
              where
                firstModules = init ms
                lastModule = last ms
                firstModuleLines = concatMap (go (True:prefixFlags)) firstModules
                lastModuleLines = go (False:prefixFlags) lastModule
        lines = case (declLines,subModuleLines) of
            ([],[]) -> [firstLine,prefix]
            (ds,[]) -> firstLine : [prefix ++ "|"] ++ ds ++ [prefix]
            ([],ms) -> firstLine : [prefix ++ "|"] ++ ms ++ [prefix]
            (ds,ms) -> firstLine : [prefix ++ "|"] ++ ds ++ [prefix ++ "|"] ++ ms ++ [prefix]

moduleNameCompletion :: ViewerMonad m => String -> (ViewerStateT m) (Maybe [Completion])
moduleNameCompletion s = do
    p <- hasOpenedProject
    if not p
        then return Nothing
        else do
            r <- runExceptT $ do
                mods <- getModules
                let modNames = catMaybes $ for mods  $ \case
                        (ModuleInfo (Symbol sym)) -> Just sym
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
declarationNameCompletion :: ViewerMonad m => String -> (ViewerStateT m) (Maybe [Completion])
declarationNameCompletion s = do
    mayben <- gets currentModule
    case mayben of
        Nothing -> return Nothing
        Just n -> do
            r <- runExceptT $ do
                m <- getModule (ModuleInfo (Symbol n))
                let infos = map (Declaration.info . item) $ Module.getDeclarations m
                let syms = for infos $ \(DeclarationInfo (Symbol sym)) -> sym
                let matchingSyms = filter (s `isPrefixOf`) syms
                return $ Just $ for matchingSyms $ \sym -> 
                    Completion{ replacement = drop (length s) sym
                              , display = sym
                              , isFinished = not $ any (\sym' -> sym `isPrefixOf` sym' && sym /= sym') matchingSyms
                              }    
            return $ case r of
                Right x -> x
                Left _ -> Nothing



data Command u m
    = Command
    { helpLine :: String
    , root :: String
    , parser :: ParsecT String u m String
    , isAllowed :: m Bool
    , completion :: String -> m (Maybe [Completion])
    , action :: String -> CommandT u m String
    }

helpCmd :: ViewerMonad m => Command u (ViewerStateT m) 
helpCmd = Command
    { helpLine = "help: show this message"
    , root = "help"
    , parser = parseArity0 "help"
    , isAllowed = return True
    , completion = \_ -> return $ Just []
    , action = \_ -> doHelp
    }


openCmd :: (MonadIO m, ViewerMonad m) => Command u (ViewerStateT m) 
openCmd = Command
    { helpLine = "open PATH: open a project rooted at PATH"
    , root = "open"
    , parser = parseArity1 "open" "directory path"
    , isAllowed = return True
    , completion =  liftM Just . listFiles
    , action = liftCmd . doOpen
    }

saveCmd :: (MonadIO m, ViewerMonad m) => Command u (ViewerStateT m)  
saveCmd = Command
    { helpLine = "save: save the project at the current path"
    , root = "save"
    , parser = parseArity0 "save"
    , isAllowed = hasOpenedProject
    , completion = liftM Just . listFiles
    , action = \_ -> liftCmd doSave
    }

saveAsCmd :: (MonadIO m, ViewerMonad m) => Command u (ViewerStateT m) 
saveAsCmd = Command
    { helpLine = "save as PATH: save the project at PATH"
    , root = "save as"
    , parser = parseArity1 "save as" "path"
    , isAllowed = hasOpenedProject
    , completion = liftM Just . listFiles
    , action = liftCmd . doSaveAs
    }

modulesCmd :: ViewerMonad m => Command u (ViewerStateT m) 
modulesCmd = Command
    { helpLine = "modules: show a list of modules in the current project"
    , root = "modules"
    , parser = parseArity0 "modules"
    , isAllowed = hasOpenedProject
    , completion = moduleNameCompletion
    , action = \_ -> liftCmd doModules
    }

moduleCmd :: ViewerMonad m => Command u (ViewerStateT m) 
moduleCmd = Command
    { helpLine = "module MODULE: set MODULE as the current module"
    , root = "module"
    , parser = parseArity1 "module" "module name"
    , isAllowed = hasOpenedProject
    , completion = \_ -> return $  Just []
    , action = liftCmd . doModule
    }

declarationsCmd :: ViewerMonad m => Command u (ViewerStateT m) 
declarationsCmd = Command
    { helpLine = "declarations: list the declarations in the current module"
    , root = "declarations"
    , parser = parseArity0 "declarations"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doDeclarations
    }

importsCmd :: ViewerMonad m => Command u (ViewerStateT m) 
importsCmd = Command
    { helpLine = "imports: list the imports in the current module"
    , root = "imports"
    , parser = parseArity0 "imports"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doImports
    }

importedCmd :: ViewerMonad m => Command u (ViewerStateT m) 
importedCmd = Command
    { helpLine = "imported: list the symbols imported by the current module"
    , root = "imported"
    , parser = parseArity0 "imported"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doImported
    }

exportsCmd :: ViewerMonad m => Command u (ViewerStateT m) 
exportsCmd = Command
    { helpLine = "exports: list the exports in the current module"
    , root = "exports"
    , parser = parseArity0 "exports"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doExports
    }

exportedCmd :: ViewerMonad m => Command u (ViewerStateT m) 
exportedCmd = Command
    { helpLine = "exported: list the symbols exported by the current module"
    , root = "exported"
    , parser = parseArity0 "exported"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doExported
    }

visibleCmd :: ViewerMonad m => Command u (ViewerStateT m) 
visibleCmd = Command
    { helpLine = "visible: list the symbols visible at the top level of the current module"
    , root = "visible"
    , parser = parseArity0 "visible"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doVisible
    }

catCmd :: ViewerMonad m => Command u (ViewerStateT m) 
catCmd = Command
    { helpLine = "cat SYMBOL: show the body of the declaration of SYMBOL"
    , root = "cat"
    , parser = parseArity1 "cat" "symbol name"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doCat
    }

editCmd :: (MonadIO m, ViewerMonad m) => (forall u . Editor m u) -> Command u (ViewerStateT m)
editCmd editor = Command
    { helpLine = "edit SYMBOL: edit a declaration"
    , root = "edit"
    , parser = parseArity1 "edit" "symbol name"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doEdit editor
    }

treeCmd :: ViewerMonad m => Command u (ViewerStateT m) 
treeCmd = Command
    { helpLine = "tree: Display a tree of the current project's modules and declarations"
    , root = "tree"
    , parser = parseArity0 "tree"
    , isAllowed = hasOpenedProject
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doTree
    }

searchCmd :: ViewerMonad m => Command u (ViewerStateT m)
searchCmd = Command
    { helpLine = "search SYMBOL: Search all modules for a declaration"
    , root = "search"
    , parser = parseArity1 "search" "search string"
    , isAllowed = hasOpenedProject
    , completion = \_ -> return $ Just []
    , action = liftCmd . doSearch
    }

quitCmd :: ViewerMonad m => Command u (ViewerStateT m) 
quitCmd = Command
    { helpLine = "quit: exit the program"
    , root = "quit"
    , parser = parseArity0 "quit"
    , isAllowed = return True
    , completion = \_ -> return $  Just []
    , action = \_ -> doQuit
    }



execCommand :: Monad m => String -> u -> CommandT u m String
execCommand input u = do
    r <- runExceptT $ parseInputT input u >>= lift . uncurry action
    return $ case r of
        Right x -> x
        Left x -> x

parseCommandT :: Monad m => Command u m -> ParsecT String u m (Command u m,String)
parseCommandT cmd = do
    arg <- parser cmd
    return (cmd,arg)

parseInputT :: Monad m => String -> u -> ExceptT String (CommandT u m) (Command u m,String)
parseInputT input u = do
    cmds <- lift getCommands
    let allCmdParsers = foldl (<|>) empty $ map parseCommandT cmds
        allParsers = allCmdParsers <|> parseGarbage
    result <- lift $ liftCmd $ P.runParserT allParsers u  "" input
    case result of
        Left err -> throwE $ lastError err
        Right cmd -> return cmd

isPrefixOfCommand :: Monad m => String -> CommandT u m [Command u m]
isPrefixOfCommand l =
    liftM (mapMaybe
            $ \x -> if l `isPrefixOf` root x && l /= root x 
                        then Just x 
                        else Nothing
          ) getCommands
      

isCommandPrefixOf :: Monad m => String -> CommandT u m [(Command u m, String)]
isCommandPrefixOf l' = 
    liftM (mapMaybe
            $ \x -> if root x == l 
                then Just (x,r) 
                else Nothing
          ) getCommands
  where
    (l,r) = case words l' of
        [] -> ("","")
        (x:xs) -> (x,unwords xs)

cmdPrefixCompletion :: Monad m => String -> CommandT u m (Maybe [Completion])
cmdPrefixCompletion l = do
    matches <- isPrefixOfCommand l
    case matches of
          [] -> return Nothing
          xs -> liftM Just $ forM xs $ \x -> do
            let replacement' = drop (length l) $ root x
                display' = root x
            isFinished' <- liftM null $ isPrefixOfCommand (root x)
            return $ Completion
                   { replacement=replacement'
                   , display=display'
                   , isFinished=isFinished'
                   }

isCommandAllowed :: Monad m => String -> CommandT u m Bool
isCommandAllowed s = do
    cmds <- getCommands
    liftMCmd (foldl (||) False) $ forM cmds $ \cmd -> liftMCmd ((root cmd == s) &&) (liftCmd $ isAllowed cmd)

cmdCompletion :: Monad m => (String,String) -> CommandT u m (String, [Completion])
cmdCompletion (l',_) = do
    matches <- cmdPrefixCompletion l
    case matches of
        Just cs -> do
            cs' <- filterM (isCommandAllowed <=< return . (l ++) . replacement) cs
            return (l',cs')
        Nothing -> do
            cs <- isCommandPrefixOf l
            cs' <- filterM (isCommandAllowed <=< return . root . fst) cs
            cs'' <- liftCmd $ forM cs' $ uncurry completion
            let cs''' = concat $ catMaybes cs''
            return (l',cs''')
  where
    l = reverse l'
    
printHelp :: Monad m => CommandT u m String
printHelp = liftM 
    ( intercalate "\n" 
    . ("Commands:" :) 
    . ("" :) 
    . map helpLine
    ) getCommands
          


