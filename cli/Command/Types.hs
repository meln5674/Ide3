module Command.Types where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P

import System.Console.Haskeline


--newtype CommandT u m a = CommandT (ReaderT [Command u m] (StateT Bool m) a)
-- | Command transformer
type CommandT u m = ReaderT [Command u m] (StateT Bool m)

-- | Type used to be able to return strings or showables
newtype Output a = MkOutput (Either a String)


-- | Create an Output from a showable
asShow :: Show a => a -> Output a
asShow = MkOutput . Left

-- | Create a list of Outputs from a list showables
asShows :: Show a => [a] -> [Output a]
asShows = map asShow

-- | Create an Output from a string
asString :: String -> Output a
asString = MkOutput . Right

-- | Create a list of Outputs from a list of strings
asStrings :: [String] -> [Output a]
asStrings = map asString

-- | Instance of show for Output which uses the default show function if not a string
instance Show a => Show (Output a) where
    show (MkOutput (Left a)) = show a
    show (MkOutput (Right a)) = a

-- | Data type which contains everything needed to use a command
data Command u m
    = Command
    { -- | Line to print in the help message
      helpLine :: String
      -- | The root of the command, without any arguments
    , root :: String
      -- | A parsec parser for the command that returns its argument
    , parser :: ParsecT String u m String
      -- | Monad action to determine if the command is allowed to run
    , isAllowed :: m Bool
      -- | Completion function
    , completion :: String -> m (Maybe [Completion])
      -- | The action to execute
    , action :: String -> CommandT u m String
    }
