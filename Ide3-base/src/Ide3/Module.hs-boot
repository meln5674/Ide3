module Ide3.Module where

import Ide3.Types
import Ide3.Monad

info :: Module -> ModuleInfo
exportedSymbols :: SolutionM m 
                => ProjectInfo
                -> Module 
                -> SolutionResult m u [ModuleChild Symbol]
symbolTree :: SolutionM m 
           => ProjectInfo
           -> Module 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
allSymbols :: Module -> [ModuleChild Symbol]
importsModule :: Module -> Symbol -> Bool
infoMatches :: Module -> ModuleInfo -> Bool
internalSymbols :: SolutionM m 
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [Symbol]
