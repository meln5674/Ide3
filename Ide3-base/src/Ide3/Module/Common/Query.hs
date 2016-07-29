module Ide3.Module.Common.Query where

import Ide3.Types
import Ide3.Monad

import Ide3.Module.Common.Types

import qualified Ide3.Module.Query as Local
import qualified Ide3.Module.Extern as Extern

-- | Get the symbols exported by either a local or external module, annotated
-- with that module's identifying information
exportedSymbols :: SolutionM m 
                => ProjectInfo
                -> EitherModule 
                -> SolutionResult m u [ModuleChild Symbol]
exportedSymbols pi m = case m of
    Left m -> Local.exportedSymbols pi m
    Right m -> return $ Extern.exportedSymbols m

-- | Given a symbol such as a class method of data constructor, find the rest
-- of the related symbols exported by a local or external module.
-- The result list's head is the data type/class, and the tail as the other
-- symbols
-- This does not return the symbol provided
symbolTree :: SolutionM m 
           => ProjectInfo
           -> EitherModule 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
symbolTree pi m s = case m of
    Left m -> Local.symbolTree pi m s
    Right m -> Extern.symbolTree m s
