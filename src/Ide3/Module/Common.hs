module Ide3.Module.Common where

import Ide3.Types
import Ide3.Monad
import {-# SOURCE #-} qualified Ide3.Module as Local
import qualified Ide3.Module.Extern as Extern

type EitherModule = Either Module ExternModule

exportedSymbols :: ProjectM m => EitherModule -> ProjectResult m [ModuleChild Symbol]
exportedSymbols (Left m) = Local.exportedSymbols m
exportedSymbols (Right m) = return $ Extern.exportedSymbols m

symbolTree :: ProjectM m => EitherModule -> Symbol -> ProjectResult m [ModuleChild Symbol]
symbolTree (Left m) s = Local.symbolTree m s
symbolTree (Right m) s = Extern.symbolTree m s
