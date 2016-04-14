module Ide3.Module where

import Control.Monad.Trans.Except

import Ide3.Types
import Ide3.Monad

info :: Module -> ModuleInfo
exportedSymbols :: ProjectM m => Module -> ProjectResult m u [ModuleChild Symbol]
symbolTree :: ProjectM m => Module -> Symbol -> ProjectResult m u [ModuleChild Symbol]
allSymbols :: Module -> [ModuleChild Symbol]
importsModule :: Module -> Symbol -> Bool
infoMatches :: Module -> ModuleInfo -> Bool
internalSymbols :: ProjectM m => Module -> ProjectResult m u [Symbol]
