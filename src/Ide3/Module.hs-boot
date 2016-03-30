module Ide3.Module where

import Control.Monad.Trans.Maybe

import Ide3.Types
import Ide3.Monad

exportedSymbols :: ProjectM m => Module -> m [ModuleChild Symbol]
symbolTree :: ProjectM m => Module -> Symbol -> MaybeT m [ModuleChild Symbol]
