module Ide3.Module.Common where

import Ide3.Types
import Ide3.Monad

type EitherModule = Either Module ExternModule

exportedSymbols :: ProjectM m => EitherModule -> ProjectResult m [ModuleChild Symbol]

symbolTree :: ProjectM m => EitherModule -> Symbol -> ProjectResult m [ModuleChild Symbol]
