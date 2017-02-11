{-|
Module      : Ide3.NewMonad.Utils
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
{-# LANGUAGE RecordWildCards #-}
module Ide3.NewMonad.Utils 
    ( module Ide3.NewMonad.Utils
    ) where

import Data.Text (Text)

import qualified Data.Map as Map
import qualified Ide3.OrderedMap as OMap

import Control.Monad.Trans.Except

import Ide3.SrcLoc

import Ide3.Types.Internal 
import Ide3.Types.State
import qualified Ide3.Module as Module 
import qualified Ide3.Import.Parser as Import 
import qualified Ide3.Export as Export 
import qualified Ide3.Declaration as Declaration

import Ide3.NewMonad

-- | Parse an import and add it to a module
addRawImport :: (ModuleImportClass m)
             => ProjectInfo 
             -> ModuleInfo 
             -> Text
             -> SolutionResult u m ImportId
addRawImport pji mi str = case Import.parse str of
    Right i -> addImport pji mi $ WithBody i str
    Left err -> throwE err 

-- | Parse an export and add it to a module
addRawExport :: (ModuleExportClass m)
             => ProjectInfo 
             -> ModuleInfo 
             -> Text
             -> SolutionResult u m ExportId
addRawExport pji mi str = case Export.parse str of
    Right e -> addExport pji mi $ WithBody e str
    Left err -> throwE err

-- | Parse a declaration and add it to a module
addRawDeclaration :: (ModuleDeclarationClass m)
                  => ProjectInfo 
                  -> ModuleInfo 
                  -> Text
                  -> SolutionResult u m ()
addRawDeclaration pji mi str = case Declaration.parse str of
    Right d -> addDeclaration pji mi $ WithBody d str
    Left err -> throwE err


addRawModuleGeneric :: ( ProjectModuleClass m
                       , ModuleDeclarationClass m
                       , ModuleImportClass m
                       , ModuleExportClass m
                       , ModulePragmaClass m
                       ) 
                    => (ModuleInfo -> SolutionResult u m ())
                    -> ProjectInfo 
                    -> String
                    -> Maybe FilePath 
                    -> Maybe ModuleInfo
                    -> SolutionResult u m (ModuleInfo, Maybe (SrcLoc,String))
addRawModuleGeneric addMod pji str p oldMi = case Module.parse str p oldMi of
    Right (m,_,_,err) -> do
        let mi = Module.info m
        addMod mi
        case m of
            Module{..} -> do
                mapM_ (addDeclaration pji mi) $ OMap.elems moduleDeclarations
                mapM_ (addImport pji mi) $ Map.elems moduleImports
                maybe (exportAll pji mi) (mapM_ $ addExport pji mi) moduleExports
                mapM_ (addPragma pji mi) modulePragmas
                editModuleHeader pji mi $ const moduleHeader
            UnparsableModule{..} -> 
                setModuleUnparsable pji mi moduleContents moduleErrorLoc moduleErrorMsg
        return (mi, err)
    Left err -> throwE err

-- | Parse an entire module and add it to the project
addRawModule :: ( ProjectModuleClass m
                , ModuleDeclarationClass m
                , ModuleImportClass m
                , ModuleExportClass m
                , ModulePragmaClass m
                ) 
             => ProjectInfo 
             -> String
             -> Maybe FilePath 
             -> Maybe ModuleInfo
             -> SolutionResult u m (ModuleInfo, Maybe (SrcLoc,String))
addRawModule pji = addRawModuleGeneric (createModule pji) pji

-- | Parse an entire module and add it to the project
updateRawModule :: ( ProjectModuleClass m
                   , ModuleDeclarationClass m
                   , ModuleImportClass m
                   , ModuleExportClass m
                   , ModulePragmaClass m
                   ) 
                => ProjectInfo 
                -> String 
                -> Maybe FilePath
                -> Maybe ModuleInfo
                -> SolutionResult u m (ModuleInfo, Maybe (SrcLoc,String))
updateRawModule pji = addRawModuleGeneric (setModuleParsable pji) pji

