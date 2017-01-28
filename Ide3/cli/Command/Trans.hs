{-# LANGUAGE OverloadedStrings #-}
module Command.Trans where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Monoid

import Data.Maybe

import Control.Applicative hiding ((<|>))

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Text.Parsec
import qualified Text.Parsec as P

import System.Console.Haskeline.Completion

import CmdParser
import Command.Types

-- | Run an action in the CommandT transformer with a set of commands
runCommandT :: Monad m => CommandT u m a -> [Command u m] -> m a
runCommandT f = flip evalStateT True . runReaderT f

-- | Run an action in the CommandT transformer with a set of commands an an
-- initial quit state
runCommandT' :: CommandT u m a -> Bool -> [Command u m] -> m (a,Bool)
runCommandT' f flag = flip runStateT flag . runReaderT f

-- Get the list of commands inside the CommandT transformer
getCommands :: Monad m => CommandT u m [Command u m]
getCommands = ask

-- | Check if the CommandT transformer is set to quit
getExitFlag :: Monad m => CommandT u m Bool
getExitFlag = lift get

-- | Set the CommandT transformer to quit
setExitFlag :: Monad m => CommandT u m ()
setExitFlag = lift $ put False

-- | Lift a command into the CommandT transformer
liftCmd :: Monad m => m a -> CommandT u m a
liftCmd = lift . lift

-- | Lift a pure function into the CommandT transformer
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

-- | Take an input string and a user state and return either an error message
-- or the response from the command executed in response
execCommand :: Monad m => Text -> u -> CommandT u m Text
execCommand input u = do
    r <- runExceptT $ parseInputT input u >>= lift . uncurry action
    return $ case r of
        Right x -> x
        Left x -> x

-- | Take a command and create a parser for it and its argument
parseCommandT :: Command u m -> ParsecT Text u m (Command u m,Text)
parseCommandT cmd = do
    arg <- parser cmd
    return (cmd,arg)

-- | Parse an input string with a start state using all availible commands
parseInputT :: Monad m => Text -> u -> ExceptT Text (CommandT u m) (Command u m,Text)
parseInputT input u = do
    cmds <- lift getCommands
    let allCmdParsers = foldl (<|>) empty $ map parseCommandT cmds
        allParsers = allCmdParsers <|> parseGarbage
    result <- lift $ liftCmd $ P.runParserT allParsers u  "" input
    case result of
        Left err -> throwE $ lastError err
        Right cmd -> return cmd

-- | Check if a string is a prefix of any command's root
isPrefixOfCommand :: Monad m => Text -> CommandT u m [Command u m]
isPrefixOfCommand l =
    liftM (mapMaybe
            $ \x -> if l `T.isPrefixOf` root x && l /= root x 
                        then Just x 
                        else Nothing
          ) getCommands
      

-- | Check if a command's root is a prefix of a string
isCommandPrefixOf :: Monad m => Text -> CommandT u m [(Command u m, Text)]
isCommandPrefixOf l' = 
    liftM (mapMaybe
            $ \x -> if root x == l 
                then Just (x,r) 
                else Nothing
          ) getCommands
  where
    (l,r) = case T.words l' of
        [] -> ("","")
        (x:xs) -> (x,T.unwords xs)

-- | Do command completion for all availible command roots
cmdPrefixCompletion :: Monad m => Text -> CommandT u m (Maybe [Completion])
cmdPrefixCompletion l = do
    matches <- isPrefixOfCommand l
    case matches of
          [] -> return Nothing
          xs -> liftM Just $ forM xs $ \x -> do
            let replacement' = T.drop (T.length l) $ root x
                display' = root x
            isFinished' <- liftM null $ isPrefixOfCommand (root x)
            return   Completion
                   { replacement=T.unpack replacement'
                   , display=T.unpack display'
                   , isFinished=isFinished'
                   }

-- | Check if a string represents a command which can be executed
isCommandAllowed :: Monad m => Text -> CommandT u m Bool
isCommandAllowed s = do
    cmds <- getCommands
    liftMCmd or $ forM cmds $ \cmd -> liftMCmd ((root cmd == s) &&) (liftCmd $ isAllowed cmd)

-- | Do command completion for all availible commands
cmdCompletion :: Monad m => (String,String) -> CommandT u m (String, [Completion])
cmdCompletion (l',_) = do
    matches <- cmdPrefixCompletion l
    case matches of
        Just cs -> do
            cs' <- filterM (isCommandAllowed <=< return . (l <>) . T.pack . replacement) cs
            return (l',cs')
        Nothing -> do
            cs <- isCommandPrefixOf l
            cs' <- filterM (isCommandAllowed <=< return . root . fst) cs
            cs'' <- liftCmd $ forM cs' $ uncurry completion
            let cs''' = concat $ catMaybes cs''
            return (l',cs''')
  where
    l = T.reverse $ T.pack l'
    
-- | Produce the help message
printHelp :: Monad m => CommandT u m Text
printHelp = liftM 
    ( T.intercalate "\n" 
    . ("Commands:" :) 
    . ("" :) 
    . map helpLine
    ) getCommands
