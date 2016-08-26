{-|
Module      : Ide3.Solution
Description : Top level operations on the solution data structure
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.Solution 
    ( module Ide3.Solution
    , module Ide3.Solution.Query
    ) where

import qualified Data.Map as Map

import Ide3.Types.Internal
import Ide3.Types.State

import Ide3.Solution.Query

-- |Create an empry Solution
empty :: Solution
empty = Solution (SolutionInfo "") Map.empty

-- |Create a new Solution from a SolutionInfo
new :: SolutionInfo -> Solution
new i = Solution i Map.empty
