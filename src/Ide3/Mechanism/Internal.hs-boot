module Ide3.Mechanism.Internal where

import Ide3.Monad
import Ide3.Types

getExternalSymbols :: ProjectM m => ModuleInfo -> ProjectResult m u [Symbol]
getAnyModule :: ProjectM m => ModuleInfo -> ProjectResult m u (Either Module ExternModule)
