module Ide3.Module.Common where

import Ide3.Types
import Ide3.Monad
import {-# SOURCE #-} qualified Ide3.Module as Local
import qualified Ide3.Module.Extern as Extern

type EitherModule = Either Module ExternModule

info :: EitherModule -> ModuleInfo
info (Right m) = Extern.info m
info (Left m) = Local.info m

exportedSymbols :: SolutionM m 
                => ProjectInfo
                -> EitherModule 
                -> SolutionResult m u [ModuleChild Symbol]
exportedSymbols pi m = case m of
    Left m -> Local.exportedSymbols pi m
    Right m -> return $ Extern.exportedSymbols m

symbolTree :: SolutionM m 
           => ProjectInfo
           -> EitherModule 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
symbolTree pi m s = case m of
    Left m -> Local.symbolTree pi m s
    Right m -> Extern.symbolTree m s
