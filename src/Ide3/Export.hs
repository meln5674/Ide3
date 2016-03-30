module Ide3.Export where

import Ide3.Types

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax hiding (Module)

import Ide3.Monad

parse :: String -> Either String Export
parse = undefined

symbolsProvided :: ProjectM m => Module -> Export -> m [Symbol]
symbolsProvided = undefined
