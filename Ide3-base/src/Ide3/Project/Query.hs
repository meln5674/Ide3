{-|
Module      : Ide3.Project.Query
Description : Queries on projects
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Project.Query where

import Ide3.Types
import Ide3.NewMonad
import Ide3.Query

-- | Find declarations in a project which provide a symbol
findSymbol :: (SolutionMonad m)
                  => ProjectInfo
                  -> Symbol
                  -> SolutionResult u m [ModuleChild [DeclarationInfo]]
findSymbol = projectFindSymbol
