{-|
Module      : Ide3.Solution.Query
Description : Queries on solutions
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Solution.Query where

import Ide3.Types
import Ide3.NewMonad
import Ide3.Query

-- | Find declrations in the solution which provide a symbol
findSymbol :: (SolutionMonad m)
                   => Symbol
                   -> SolutionResult u m
                      [ProjectChild 
                        [ModuleChild 
                          [DeclarationInfo]]]
findSymbol = solutionFindSymbol
