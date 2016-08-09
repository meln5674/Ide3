module Ide3.NewMonad.Utils 
    ( module Ide3.NewMonad.Utils
    , getInternalSymbols
    , getExternalSymbols
    ) where

import qualified Data.Map as Map
import qualified Ide3.OrderedMap as OMap

import Control.Monad
import Control.Monad.Trans.Except

import Ide3.Types.Internal 
import qualified Ide3.Module as Module 
import qualified Ide3.Import.Parser as Import 
import qualified Ide3.Export as Export 
import qualified Ide3.Declaration as Declaration

import Ide3.NewMonad

import Ide3.Query (getInternalSymbols, getExternalSymbols)

-- | Parse an import and add it to a module
addRawImport :: (ModuleImportClass m)
             => ProjectInfo 
             -> ModuleInfo 
             -> String -> SolutionResult m u ImportId
addRawImport pji mi str = case Import.parse str of
    Right i -> addImport pji mi $ WithBody i str
    Left err -> throwE err 

-- | Parse an export and add it to a module
addRawExport :: (ModuleExportClass m)
             => ProjectInfo 
             -> ModuleInfo 
             -> String -> SolutionResult m u ExportId
addRawExport pji mi str = case Export.parse str of
    Right e -> addExport pji mi $ WithBody e str
    Left err -> throwE err

-- | Parse a declaration and add it to a module
addRawDeclaration :: (ModuleDeclarationClass m)
                  => ProjectInfo 
                  -> ModuleInfo 
                  -> String 
                  -> SolutionResult m u ()
addRawDeclaration pji mi str = case Declaration.parse str of
    Right d -> addDeclaration pji mi $ WithBody d str
    Left err -> throwE err

-- | Parse an entire module and add it to the project
addRawModule :: ( ProjectModuleClass m
                , ProjectExternModuleClass m
                , ModuleDeclarationClass m
                , ModuleImportClass m
                , ModuleExportClass m
                , ModulePragmaClass m
                ) 
             => ProjectInfo 
             -> String 
             -> Maybe FilePath 
             -> SolutionResult m u ModuleInfo
addRawModule pji str p = case Module.parse str p of
    Right (m,_,_) -> do
        --addModule pji m
        let mi = Module.info m
        createModule pji mi
        mapM (addDeclaration pji mi) $ OMap.elems $ moduleDeclarations m
        mapM (addImport pji mi) $ Map.elems $ moduleImports m
        maybe (exportAll pji mi) (mapM_ $ addExport pji mi) $ moduleExports m
        mapM (addPragma pji mi) $ modulePragmas m
        editModuleHeader pji mi $ const $ moduleHeader m
        return mi
    Left err -> throwE err

