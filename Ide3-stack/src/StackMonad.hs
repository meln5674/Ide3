module StackMonad where

import Data.Text (Text)

import Ide3.Types

class (Monad m) => StackMonad m where
    getStackConfig :: SolutionResult u m Text
    setStackConfig :: Text -> SolutionResult u m ()
