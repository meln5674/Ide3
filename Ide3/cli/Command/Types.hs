{-# LANGUAGE OverloadedStrings #-}
module Command.Types where

import Data.Text (Text)
import qualified Data.Text as T

import Data.List

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Text.Parsec hiding (parse)

import System.Console.Haskeline


--newtype CommandT u m a = CommandT (ReaderT [Command u m] (StateT Bool m) a)
-- | Command transformer
type CommandT u m = ReaderT [Command u m] (StateT Bool m)

-- | Type used to be able to return strings or showables
newtype Output a = MkOutput (Either a Text)


-- | Create an Output from a showable
asShow :: a -> Output a
asShow = MkOutput . Left

-- | Create a list of Outputs from a list showables
asShows :: [a] -> [Output a]
asShows = map asShow

-- | Create an Output from a string
asString :: Text -> Output a
asString = MkOutput . Right

-- | Create a list of Outputs from a list of strings
asStrings :: [Text] -> [Output a]
asStrings = map asString

defaultOutput :: Output a -> Output Text
defaultOutput (MkOutput (Left _)) = error "CANNOT DEFAULT"
defaultOutput (MkOutput (Right x)) = MkOutput (Right x)

defaultOutputs :: [Output a] -> [Output Text]
defaultOutputs = map defaultOutput

processOutputs :: (Show a) => [Output a] -> Text
processOutputs = T.intercalate "\n" . map (T.pack . show)

-- | Instance of show for Output which uses the default show function if not a string
instance Show a => Show (Output a) where
    show (MkOutput (Left a)) = show a
    show (MkOutput (Right a)) = T.unpack a

-- | Data type which contains everything needed to use a command
data Command u m
    = Command
    { -- | Line to print in the help message
      helpLine :: Text
      -- | The root of the command, without any arguments
    , root :: Text
      -- | A parsec parser for the command that returns its argument
    , parser :: ParsecT Text u m Text
      -- | Monad action to determine if the command is allowed to run
    , isAllowed :: m Bool
      -- | Completion function
    , completion :: Text -> m (Maybe [Completion])
      -- | The action to execute
    , action :: Text -> CommandT u m Text
    }
