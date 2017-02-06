{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Ide3.Module.Lens where

import Ide3.OrderedMap (OrderedMap)
import qualified Ide3.OrderedMap as OM

import Data.Text (Text)

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Control.Lens

import qualified Ide3.Types.Internal as T
import qualified Ide3.Types.State as T
import qualified Ide3.SrcLoc as T

import qualified Ide3.Module as Module

type instance Index (OrderedMap k v) = k
type instance IxValue (OrderedMap k v) = v

instance Ord k => At (OrderedMap k v) where
    at k f m = f mv <&> \case
        Nothing -> maybe m (const (OM.delete k m)) mv
        Just v' -> OM.insert' k v' m
        where mv = OM.lookup k m

instance Ord k => Ixed (OrderedMap k v) where
    ix k f m = case OM.lookup k m of
        Just v -> f v <&> \v' -> OM.insert' k v' m
        Nothing -> pure m

newtype ParsableModule = MkParsableModule { unParsableModule :: T.Module }

newtype UnparsableModule = MkUnparsableModule { unUnparsableModule :: T.Module }

asParsable :: Lens' T.Module ParsableModule
asParsable = lens getter setter
  where
    getter T.UnparsableModule{ T.moduleInfo } = MkParsableModule $ Module.new moduleInfo
    getter m = MkParsableModule m
    setter _ = unParsableModule

withParsable :: Iso' ParsableModule T.Module
withParsable = iso unParsableModule MkParsableModule

moduleHeader :: Lens' ParsableModule Text
moduleHeader = withParsable 
             . lens T.moduleHeader (\module_ moduleHeader -> module_ { T.moduleHeader } )

modulePragmas :: Lens' ParsableModule [T.Pragma]
modulePragmas = withParsable 
              . lens T.modulePragmas (\module_ modulePragmas -> module_ { T.modulePragmas } )

moduleImports :: Lens' ParsableModule T.ImportCollection
moduleImports = withParsable 
              . lens T.moduleImports (\module_ moduleImports -> module_ { T.moduleImports } )

moduleExports :: Lens' ParsableModule T.ExportCollection
moduleExports = withParsable 
              . lens T.moduleExports (\module_ moduleExports -> module_ { T.moduleExports } )

moduleExports' :: Lens' ParsableModule (Map T.ExportId (T.WithBody T.Export))
moduleExports' = moduleExports 
               . non M.empty

moduleDeclarations :: Lens' ParsableModule T.DeclarationCollection
moduleDeclarations = withParsable 
                   . lens T.moduleDeclarations (\module_ moduleDeclarations -> module_ { T.moduleDeclarations } )

newtype ModulePragmas = MkModulePragmas { unModulePragmas :: ParsableModule }

type instance Index ModulePragmas = Int
type instance IxValue ModulePragmas = T.Pragma

withPragmas :: Iso' ModulePragmas ParsableModule
withPragmas = iso unModulePragmas MkModulePragmas

overPragmas :: Iso' ParsableModule ModulePragmas
overPragmas = iso MkModulePragmas unModulePragmas

instance Ixed ModulePragmas where
    ix i = withPragmas 
         . modulePragmas
         . ix i

getPragmas :: Getter ParsableModule [T.Pragma]
getPragmas = modulePragmas



newtype ModuleDeclarations = MkModuleDeclarations { unModuleDeclarations :: ParsableModule }

type instance Index ModuleDeclarations = T.DeclarationInfo
type instance IxValue ModuleDeclarations = T.WithBody T.Declaration

withDeclarations :: Iso' ModuleDeclarations ParsableModule
withDeclarations = iso unModuleDeclarations MkModuleDeclarations

overDeclarations :: Iso' ParsableModule ModuleDeclarations
overDeclarations = iso MkModuleDeclarations unModuleDeclarations

instance Ixed ModuleDeclarations where
    ix di = withDeclarations 
          . moduleDeclarations
          . ix di

instance At ModuleDeclarations where
    at di = withDeclarations 
          . moduleDeclarations
          . at di

getDeclarations :: Getter ParsableModule [T.DeclarationInfo]
getDeclarations
    = moduleDeclarations
    . to OM.keys

hasDeclaration :: Index ModuleDeclarations -> Getter T.Module Bool
hasDeclaration di = asParsable
                  . overDeclarations 
                  . at di 
                  . to (maybe False (const True))

newtype ModuleImports = MkModuleImports { unModuleImports :: ParsableModule }

type instance Index ModuleImports = T.ImportId
type instance IxValue ModuleImports = T.WithBody T.Import

withImports :: Iso' ModuleImports ParsableModule
withImports = iso unModuleImports MkModuleImports

overImports :: Iso' ParsableModule ModuleImports
overImports = iso MkModuleImports unModuleImports

instance Ixed ModuleImports where
    ix ii = withImports 
          . moduleImports 
          . ix ii

instance At ModuleImports where
    at ii = withImports 
          . moduleImports 
          . at ii

getImports :: Getter ParsableModule [T.ImportId]
getImports
    = moduleImports
    . to M.keys

hasImport :: Index ModuleImports -> Getter T.Module Bool
hasImport ii = asParsable
             . overImports 
             . at ii 
             . to (maybe False (const True))

nextImportId :: Getter ParsableModule T.ImportId
nextImportId = moduleImports . to (maybe 0 ((+1) . fst . fst) . M.maxViewWithKey)

newtype ModuleExports = MkModuleExports { unModuleExports :: ParsableModule }

type instance Index ModuleExports = T.ExportId
type instance IxValue ModuleExports = T.WithBody T.Export

withExports :: Iso' ModuleExports ParsableModule
withExports = iso unModuleExports MkModuleExports

overExports :: Iso' ParsableModule ModuleExports
overExports = iso MkModuleExports unModuleExports

instance Ixed ModuleExports where
    ix ei = withExports 
          . moduleExports' 
          . ix ei

instance At ModuleExports where
    at ei = withExports 
          . moduleExports' 
          . at ei

getExports :: Getter ParsableModule (Maybe [T.ExportId])
getExports
    = moduleExports
    . to (fmap M.keys)

hasExport :: Index ModuleExports -> Getter T.Module Bool
hasExport ei = asParsable
             . overExports 
             . at ei 
             . to (maybe False (const True))

nextExportId :: Getter ParsableModule T.ExportId
nextExportId = moduleExports' . to (maybe 0 ((+1) . fst . fst) . M.maxViewWithKey)



asUnparsable :: Lens' T.Module UnparsableModule
asUnparsable = lens getter setter
  where
    getter T.Module{ T.moduleInfo } = MkUnparsableModule $ Module.newUnparsable moduleInfo
    getter m = MkUnparsableModule m
    setter _ = unUnparsableModule

withUnparsable :: Iso' UnparsableModule T.Module
withUnparsable = iso unUnparsableModule MkUnparsableModule

moduleContents :: Lens' UnparsableModule Text
moduleContents = withUnparsable 
               . lens T.moduleContents (\module_ moduleContents -> module_ { T.moduleContents } )

moduleErrorLoc :: Lens' UnparsableModule T.SrcLoc
moduleErrorLoc = withUnparsable 
               . lens T.moduleErrorLoc (\module_ moduleErrorLoc -> module_ { T.moduleErrorLoc } )

moduleErrorMsg :: Lens' UnparsableModule String
moduleErrorMsg = withUnparsable 
               . lens T.moduleErrorMsg (\module_ moduleErrorMsg -> module_ { T.moduleErrorMsg } )

getUnparsable :: Getter T.Module (Maybe (Text, T.SrcLoc, String))
getUnparsable = to $ \case
    T.UnparsableModule{..} -> Just (moduleContents, moduleErrorLoc, moduleErrorMsg)
    _ -> Nothing
