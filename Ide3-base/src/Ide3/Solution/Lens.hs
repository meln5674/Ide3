{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Ide3.Solution.Lens where

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Control.Lens

import qualified Ide3.Types.Internal as T
import qualified Ide3.Types.State as T

import qualified Ide3.Project.Lens as Project
import qualified Ide3.Project as Project

orFail :: e -> Getter Bool (Maybe e)
orFail e = to $ \case
    True -> Nothing
    False -> Just e

thenTry :: Getter a (Maybe b) -> Getter a (Maybe b) -> Getter a (Maybe b)
thenTry f g = to $ \a -> case a ^. f of
    Just x -> Just x
    Nothing -> a ^. g


solutionInfo :: Lens' T.Solution T.SolutionInfo
solutionInfo = lens T.solutionInfo (\solution solutionInfo -> solution { T.solutionInfo } )

solutionProjects :: Lens' T.Solution (Map T.ProjectInfo T.Project)
solutionProjects = lens T.solutionProjects (\solution solutionProjects -> solution { T.solutionProjects } )

type instance Index T.Solution = T.ProjectInfo
type instance IxValue T.Solution = T.Project

instance Ixed T.Solution where
    ix pji = solutionProjects . ix pji

instance At T.Solution where
    at pji = solutionProjects . at pji

getProjects :: Getter T.Solution [T.ProjectInfo]
getProjects
    = solutionProjects
    . to M.keys

hasProject :: T.ProjectInfo -> Getter T.Solution Bool
hasProject pji
    = at pji 
    . to (maybe False (const True))

checkProject :: T.ProjectInfo -> Getter T.Solution (Maybe (T.SolutionError u))
checkProject pji
    = hasProject pji
    . orFail (T.ProjectNotFound pji "")

uncheckProject :: T.ProjectInfo -> Getter T.Solution (Maybe (T.SolutionError u))
uncheckProject pji
    = hasProject pji
    . to not
    . orFail (T.DuplicateProject pji "")

newtype SolutionLocals = MkSolutionLocals { unSolutionLocals :: T.Solution }

withLocals :: Iso' SolutionLocals T.Solution
withLocals = iso unSolutionLocals MkSolutionLocals

overLocals :: Iso' T.Solution SolutionLocals 
overLocals = iso MkSolutionLocals unSolutionLocals 

type instance Index SolutionLocals = (T.ProjectInfo, T.ModuleInfo)
type instance IxValue SolutionLocals = T.Module

instance Ixed SolutionLocals where
    ix (pji, mi)
        = withLocals 
        . ix pji 
        . Project.overLocals 
        . ix mi

instance At SolutionLocals where
    at (pji, mi) 
        = withLocals 
        . at pji . non (Project.new pji) 
        . Project.overLocals 
        . at mi

hasLocalModule :: Index SolutionLocals -> Getter T.Solution Bool
hasLocalModule (pji, mi)
    = overLocals
    . at (pji, mi)
    . to (maybe False (const True))

checkLocalModule :: Index SolutionLocals -> Getter T.Solution (Maybe (T.SolutionError u))
checkLocalModule (pji, mi) = projectCheck `thenTry` moduleCheck
  where
    projectCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    projectCheck = checkProject pji
    moduleCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    moduleCheck
        = hasLocalModule (pji, mi)
        . orFail (T.ModuleNotFound pji mi "")
  
uncheckLocalModule :: Index SolutionLocals -> Getter T.Solution (Maybe (T.SolutionError u))
uncheckLocalModule (pji, mi) = projectCheck `thenTry` moduleCheck
  where
    projectCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    projectCheck = checkProject pji
    moduleCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    moduleCheck
        = hasLocalModule (pji, mi)
        . to not
        . orFail (T.DuplicateModule pji mi "")
  
  
  
newtype SolutionPragmas = MkSolutionPragmas { unSolutionPragmas :: T.Solution }

withPragmas :: Iso' SolutionPragmas T.Solution
withPragmas = iso unSolutionPragmas MkSolutionPragmas

overPragmas :: Iso' T.Solution SolutionPragmas 
overPragmas = iso MkSolutionPragmas unSolutionPragmas 

type instance Index SolutionPragmas = (T.ProjectInfo, T.ModuleInfo, Int)
type instance IxValue SolutionPragmas = T.Pragma

instance Ixed SolutionPragmas where
    ix (pji, mi, i) 
        = withPragmas 
        . ix pji 
        . Project.overLocals 
        . Project.overPragmas 
        . ix (mi, i)


newtype SolutionDeclarations = MkSolutionDeclarations { unSolutionDeclarations :: T.Solution }

withDeclarations :: Iso' SolutionDeclarations T.Solution
withDeclarations = iso unSolutionDeclarations MkSolutionDeclarations

overDeclarations :: Iso' T.Solution SolutionDeclarations 
overDeclarations = iso MkSolutionDeclarations unSolutionDeclarations 

type instance Index SolutionDeclarations = (T.ProjectInfo, T.ModuleInfo, T.DeclarationInfo)
type instance IxValue SolutionDeclarations = T.WithBody T.Declaration

instance Ixed SolutionDeclarations where
    ix (pji, mi, di) 
        = withDeclarations 
        . ix pji 
       . Project.overLocals 
       . Project.overDeclarations 
       . ix (mi, di)

instance At SolutionDeclarations where
    at (pji, mi, di) 
        = withDeclarations 
        . at pji . non (Project.new pji) 
        . Project.overLocals 
        . Project.overDeclarations 
        . at (mi, di)


hasDeclaration :: Index SolutionDeclarations -> Getter T.Solution Bool
hasDeclaration (pji, mi, di)
    = overDeclarations
    . at (pji, mi, di)
    . to (maybe False (const True))

checkDeclaration :: Index SolutionDeclarations -> Getter T.Solution (Maybe (T.SolutionError u))
checkDeclaration (pji, mi, di) = moduleCheck `thenTry` declarationCheck
  where
    moduleCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    moduleCheck = checkLocalModule (pji, mi)
    declarationCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    declarationCheck
        = hasDeclaration (pji, mi, di)
        . orFail (T.DeclarationNotFound mi di "")
  


uncheckDeclaration :: Index SolutionDeclarations -> Getter T.Solution (Maybe (T.SolutionError u))
uncheckDeclaration (pji, mi, di) = moduleCheck `thenTry` declarationCheck
  where
    moduleCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    moduleCheck = checkLocalModule (pji, mi)
    declarationCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    declarationCheck
        = hasDeclaration (pji, mi, di)
        . to not
        . orFail (T.DuplicateDeclaration mi di "")
  


newtype SolutionImports = MkSolutionImports { unSolutionImports :: T.Solution }

withImports :: Iso' SolutionImports T.Solution
withImports = iso unSolutionImports MkSolutionImports

overImports :: Iso' T.Solution SolutionImports 
overImports = iso MkSolutionImports unSolutionImports 

type instance Index SolutionImports = (T.ProjectInfo, T.ModuleInfo, T.ImportId)
type instance IxValue SolutionImports = T.WithBody T.Import

instance Ixed SolutionImports where
    ix (pji, mi, ii) 
        = withImports 
        . ix pji 
        . Project.overLocals 
        . Project.overImports 
        . ix (mi, ii)

instance At SolutionImports where
    at (pji, mi, ii)
        = withImports 
        . at pji . non (Project.new pji) 
        . Project.overLocals 
        . Project.overImports 
        . at (mi, ii)



hasImport :: Index SolutionImports -> Getter T.Solution Bool
hasImport (pji, mi, di)
    = overImports
    . at (pji, mi, di)
    . to (maybe False (const True))

checkImport :: Index SolutionImports -> Getter T.Solution (Maybe (T.SolutionError u))
checkImport (pji, mi, di) = moduleCheck `thenTry` importCheck
  where
    moduleCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    moduleCheck = checkLocalModule (pji, mi)
    importCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    importCheck
        = hasImport (pji, mi, di)
        . orFail (T.InvalidImportId mi di "")
  


newtype SolutionExports = MkSolutionExports { unSolutionExports :: T.Solution }

withExports :: Iso' SolutionExports T.Solution
withExports = iso unSolutionExports MkSolutionExports

overExports :: Iso' T.Solution SolutionExports 
overExports = iso MkSolutionExports unSolutionExports 

type instance Index SolutionExports = (T.ProjectInfo, T.ModuleInfo, T.ExportId)
type instance IxValue SolutionExports = T.WithBody T.Export

instance Ixed SolutionExports where
    ix (pji, mi, ei)
        = withExports 
        . ix pji 
        . Project.overLocals 
        . Project.overExports 
        . ix (mi, ei)

instance At SolutionExports where
    at (pji, mi, ei) 
        = withExports 
        . at pji . non (Project.new pji) 
        . Project.overLocals 
        . Project.overExports 
        . at (mi, ei)


hasExport :: Index SolutionExports -> Getter T.Solution Bool
hasExport (pji, mi, di)
    = overExports
    . at (pji, mi, di)
    . to (maybe False (const True))

checkExport :: Index SolutionExports -> Getter T.Solution (Maybe (T.SolutionError u))
checkExport (pji, mi, di) = moduleCheck `thenTry` exportCheck
  where
    moduleCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    moduleCheck = checkLocalModule (pji, mi)
    exportCheck :: Getter T.Solution (Maybe (T.SolutionError u))
    exportCheck
        = hasExport (pji, mi, di)
        . orFail (T.InvalidExportId mi di "")
  


newtype SolutionExterns = MkSolutionExterns { unSolutionExterns :: T.Solution }

withExterns :: Iso' SolutionExterns T.Solution 
withExterns = iso unSolutionExterns MkSolutionExterns

overExterns :: Iso' T.Solution SolutionExterns 
overExterns = iso MkSolutionExterns unSolutionExterns 

type instance Index SolutionExterns = (T.ProjectInfo, T.ModuleInfo)
type instance IxValue SolutionExterns = T.ExternModule

instance Ixed SolutionExterns where
    ix (pji, mi) 
        = withExterns 
        . ix pji 
        . Project.overExterns 
        . ix mi

instance At SolutionExterns where
    at (pji, mi)
        = withExterns 
        . at pji . non (Project.new pji) 
        . Project.overExterns 
        . at mi

hasExternModule :: Index SolutionExterns -> Getter T.Solution Bool
hasExternModule (pji, mi)
    = overExterns
    . at (pji, mi)
    . to (maybe False (const True))
