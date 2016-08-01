module Ide3.Module.Query where

import Ide3.Types

import Ide3.NewMonad

importsModule :: Module -> Symbol -> Bool

exportedSymbols :: Monad m 
                => ProjectInfo
                -> Module 
                -> SolutionResult m u [ModuleChild Symbol]
symbolTree :: Monad m 
           => ProjectInfo
           -> Module 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]

internalSymbols :: Monad m 
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [Symbol]
