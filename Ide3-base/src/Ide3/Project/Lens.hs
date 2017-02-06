{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
module Ide3.Project.Lens where

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Control.Lens

import qualified Ide3.Types.Internal as T
import qualified Ide3.Types.State as T

import qualified Ide3.Module.Lens as Module
import qualified Ide3.Module as Module


projectInfo :: Lens' T.Project T.ProjectInfo
projectInfo = lens T.projectInfo (\project projectInfo -> project { T.projectInfo } )

buildInfo :: Lens' T.Project T.BuildInfo
buildInfo = lens T.projectBuildInfo (\project projectBuildInfo -> project { T.projectBuildInfo } )

projectModules :: Lens' T.Project (Map T.ModuleInfo T.Module)
projectModules = lens T.projectModules (\project projectModules -> project { T.projectModules } )

projectExternModules :: Lens' T.Project (Map T.ModuleInfo T.ExternModule)
projectExternModules = lens T.projectExternModules (\project projectExternModules -> project { T.projectExternModules } )



newtype ProjectLocals = MkProjectLocals { unProjectLocals :: T.Project }

withLocals :: Iso' ProjectLocals T.Project
withLocals = iso unProjectLocals MkProjectLocals

overLocals :: Iso' T.Project ProjectLocals
overLocals = iso MkProjectLocals unProjectLocals

type instance Index ProjectLocals = T.ModuleInfo
type instance IxValue ProjectLocals = T.Module

instance Ixed ProjectLocals where
    ix mi = withLocals 
          . projectModules 
          . ix mi
    
instance At ProjectLocals where
    at mi = withLocals 
          . projectModules 
          . at mi

getLocalModules :: Getter T.Project [T.ModuleInfo]
getLocalModules
    = projectModules
    . to M.keys

hasLocalModule :: Index ProjectLocals -> Getter T.Project Bool
hasLocalModule mi = overLocals
                  . at mi
                  . to (maybe False (const True))

newtype ProjectPragmas = MkProjectPragmas { unProjectPragmas :: ProjectLocals }

withPragmas :: Iso' ProjectPragmas ProjectLocals
withPragmas = iso unProjectPragmas MkProjectPragmas

overPragmas :: Iso' ProjectLocals ProjectPragmas
overPragmas = iso MkProjectPragmas unProjectPragmas

type instance Index ProjectPragmas = (T.ModuleInfo, Int)
type instance IxValue ProjectPragmas = T.Pragma

instance Ixed ProjectPragmas where
    ix (mi, i) = withPragmas . ix mi . Module.asParsable . Module.overPragmas . ix i


newtype ProjectDeclarations = MkProjectDeclarations { unProjectDeclarations :: ProjectLocals }

withDeclarations :: Iso' ProjectDeclarations ProjectLocals
withDeclarations = iso unProjectDeclarations MkProjectDeclarations

overDeclarations :: Iso' ProjectLocals ProjectDeclarations
overDeclarations = iso MkProjectDeclarations unProjectDeclarations

type instance Index ProjectDeclarations = (T.ModuleInfo, T.DeclarationInfo)
type instance IxValue ProjectDeclarations = T.WithBody T.Declaration

instance Ixed ProjectDeclarations where
    ix (mi, di) = withDeclarations . ix mi . Module.asParsable . Module.overDeclarations . ix di

instance At ProjectDeclarations where
    at (mi, di) = withDeclarations . at mi . non (Module.new mi) . Module.asParsable . Module.overDeclarations . at di

newtype ProjectImports = MkProjectImports { unProjectImports :: ProjectLocals }

withImports :: Iso' ProjectImports ProjectLocals
withImports = iso unProjectImports MkProjectImports

overImports :: Iso' ProjectLocals ProjectImports
overImports = iso MkProjectImports unProjectImports

type instance Index ProjectImports = (T.ModuleInfo, T.ImportId)
type instance IxValue ProjectImports = T.WithBody T.Import

instance Ixed ProjectImports where
    ix (mi, ii) = withImports . ix mi . Module.asParsable . Module.overImports . ix ii

instance At ProjectImports where
    at (mi, ii) = withImports . at mi . non (Module.new mi) . Module.asParsable . Module.overImports . at ii



newtype ProjectExports = MkProjectExports { unProjectExports :: ProjectLocals }

withExports :: Iso' ProjectExports ProjectLocals
withExports = iso unProjectExports MkProjectExports

overExports :: Iso' ProjectLocals ProjectExports
overExports = iso MkProjectExports unProjectExports

type instance Index ProjectExports = (T.ModuleInfo, T.ExportId)
type instance IxValue ProjectExports = T.WithBody T.Export

instance Ixed ProjectExports where
    ix (mi, ei) = withExports . ix mi . Module.asParsable . Module.overExports . ix ei

instance At ProjectExports where
    at (mi, ei) = withExports . at mi . non (Module.new mi) . Module.asParsable . Module.overExports . at ei

    





newtype ProjectExterns = MkProjectExterns { unProjectExterns :: T.Project }

withExterns :: Iso' ProjectExterns T.Project
withExterns = iso unProjectExterns MkProjectExterns

overExterns :: Iso' T.Project ProjectExterns
overExterns = iso MkProjectExterns unProjectExterns

type instance Index ProjectExterns = T.ModuleInfo
type instance IxValue ProjectExterns = T.ExternModule

instance Ixed ProjectExterns where
    ix mi = withExterns . projectExternModules . ix mi
    
instance At ProjectExterns where
    at mi = withExterns . projectExternModules . at mi


getExternModules :: Getter T.Project [T.ModuleInfo]
getExternModules
    = projectExternModules
    . to M.keys

hasExternModule :: Index ProjectExterns -> Getter T.Project Bool
hasExternModule mi = overExterns
                   . at mi
                   . to (maybe False (const True))
