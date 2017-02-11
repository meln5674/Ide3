{-|
Module      : Ide3.NewerMonad.Utils
Description : Operations which utilize the NewMonad typeclasses
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

The functions in this module perform operations using the typeclasses found in
Ide3.NewMonad, allowing them to work with any instance of them.
-}

{-# LANGUAGE NamedFieldPuns #-}
module Ide3.NewerMonad.Utils 
    ( module Ide3.NewerMonad.Utils
    ) where

import qualified Data.Map as Map
import qualified Ide3.OrderedMap as OMap

import Control.Monad.Except

import Ide3.SrcLoc

import Ide3.Types.Internal 
import Ide3.Types.State
import qualified Ide3.Module as Module 
import qualified Ide3.Import.Parser as Import 
import qualified Ide3.Export as Export 
import qualified Ide3.Declaration as Declaration

import Ide3.NewerMonad

-- | Parse an import and add it to a module
addRawImport :: (ModuleImportClass u m)
             => ProjectInfo 
             -> ModuleInfo 
             -> String -> m ImportId
addRawImport pji mi str = case Import.parse str of
    Right i -> addImport pji mi $ WithBody i str
    Left err -> throwError err 

-- | Parse an export and add it to a module
addRawExport :: (ModuleExportClass u m)
             => ProjectInfo 
             -> ModuleInfo 
             -> String -> m ExportId
addRawExport pji mi str = case Export.parse str of
    Right e -> addExport pji mi $ WithBody e str
    Left err -> throwError err

-- | Parse a declaration and add it to a module
addRawDeclaration :: (ModuleDeclarationClass u m)
                  => ProjectInfo 
                  -> ModuleInfo 
                  -> String 
                  -> m ()
addRawDeclaration pji mi str = case Declaration.parse str of
    Right d -> addDeclaration pji mi $ WithBody d str
    Left err -> throwError err

-- | Parse an entire module and add it to the project
addRawModule :: ( ProjectModuleClass u m
                , ModuleDeclarationClass u m
                , ModuleImportClass u m
                , ModuleExportClass u m
                , ModulePragmaClass u m
                ) 
             => ProjectInfo 
             -> String 
             -> Maybe FilePath 
             -> m (ModuleInfo, Maybe (SrcLoc,String))
addRawModule pji str p = case Module.parse str p of
    Right (m,_,_,err) -> do
        let mi = Module.info m
        createModule pji mi
        case m of
            Module { moduleDeclarations
                   , moduleImports
                   , moduleExports
                   , modulePragmas
                   , moduleHeader
                   } -> do
                mapM_ (addDeclaration pji mi) $ OMap.elems $ moduleDeclarations
                mapM_ (addImport pji mi) $ Map.elems $ moduleImports
                maybe (exportAll pji mi) (mapM_ $ addExport pji mi) $ moduleExports
                mapM_ (addPragma pji mi) $ modulePragmas
                editModuleHeader pji mi $ const $ moduleHeader
            UnparsableModule { moduleContents } -> do
                setModuleUnparsable pji mi moduleContents
        return (mi, err)
    Left err -> throwError err

-- | Parse an entire module and add it to the project
updateRawModule :: ( ProjectModuleClass u m
                   , ModuleDeclarationClass u m
                   , ModuleImportClass u m
                   , ModuleExportClass u m
                   , ModulePragmaClass u m
                   ) 
                => ProjectInfo 
                -> String 
                -> Maybe FilePath 
                -> m (ModuleInfo, Maybe (SrcLoc,String))
updateRawModule pji str p = case Module.parse str p of
    Right (m,_,_,err) -> do
        let mi = Module.info m
        setModuleParsable pji mi
        case m of
            Module { moduleDeclarations
                   , moduleImports
                   , moduleExports
                   , modulePragmas
                   , moduleHeader
                   } -> do
                mapM_ (addDeclaration pji mi) $ OMap.elems $ moduleDeclarations
                mapM_ (addImport pji mi) $ Map.elems $ moduleImports
                maybe (exportAll pji mi) (mapM_ $ addExport pji mi) $ moduleExports
                mapM_ (addPragma pji mi) $ modulePragmas
                editModuleHeader pji mi $ const $ moduleHeader
            UnparsableModule { moduleContents } -> do
                setModuleUnparsable pji mi moduleContents
        return (mi, err)
    Left err -> throwError err

