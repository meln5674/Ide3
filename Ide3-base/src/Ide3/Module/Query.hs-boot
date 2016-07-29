module Ide3.Module.Query where

import Ide3.Types

import Ide3.Monad

importsModule :: Module -> Symbol -> Bool

exportedSymbols :: SolutionM m 
                => ProjectInfo
                -> Module 
                -> SolutionResult m u [ModuleChild Symbol]
symbolTree :: SolutionM m 
           => ProjectInfo
           -> Module 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]

internalSymbols :: SolutionM m 
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [Symbol]
