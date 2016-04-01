module Ide3.Module where

import Control.Monad.Trans.Except

import Ide3.Types
import Ide3.Monad

exportedSymbols :: ProjectM m => Module -> ExceptT ProjectError m [ModuleChild Symbol]
symbolTree :: ProjectM m => Module -> Symbol -> ExceptT ProjectError m [ModuleChild Symbol]
allSymbols :: Module -> [ModuleChild Symbol]
importsModule :: Module -> Symbol -> Bool
