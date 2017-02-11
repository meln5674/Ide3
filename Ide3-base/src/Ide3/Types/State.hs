{-|
Module      : Ide3.Types.State
Description : Types used for the stateful implementation of the NewMonad
                typeclasses
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE OverloadedStrings #-}
module Ide3.Types.State where

import Data.Text (Text)

import Data.Map.Strict ( Map )

import Ide3.OrderedMap (OrderedMap)

import Ide3.Types.Internal
import Ide3.SrcLoc.Types

-- | A solution, a collection of projects
data Solution
    = Solution
    { solutionInfo :: SolutionInfo
    , solutionProjects :: Map ProjectInfo Project
    }
  deriving (Show, Read)

-- |Top level type, contains information about a project, how to build it,
--  and the modules it contains
data Project
    = Project
    { projectInfo :: ProjectInfo
    , projectModules :: Map ModuleInfo Module
    , projectBuildInfo :: BuildInfo
    , projectExternModules :: Map ModuleInfo ExternModule
    }
    deriving (Show, Read, Eq)

-- | A collection of imports
type ImportCollection = Map ImportId (WithBody Import)

-- | A collection of exports, or a mark that everything is exported
type ExportCollection = Maybe (Map ExportId (WithBody Export))

-- | A collection of declarations
type DeclarationCollection = OrderedMap DeclarationInfo (WithBody Declaration)

-- | A module. 
data Module
    = Module 
    { moduleInfo :: ModuleInfo
    , moduleHeader :: Text
    , modulePragmas :: [Pragma]
    , moduleImports :: ImportCollection
    , moduleExports :: ExportCollection
    , moduleDeclarations :: DeclarationCollection
    }
    | UnparsableModule
    { moduleInfo :: ModuleInfo
    , moduleContents :: Text
    , moduleErrorLoc :: SrcLoc 
    , moduleErrorMsg :: String
    }
    deriving (Show, Read, Eq)

-- | A module which is external to the project, only a list of exported symbols
--  are availible
data ExternModule
    = ExternModule
    { externModuleInfo :: ModuleInfo
    , externModuleExports :: Map ExportId ExternExport
    }
    deriving (Show, Eq, Read, Ord)
