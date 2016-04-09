{-|
Module      : Ide3.Mechanism.Internal
Description : Convienience functions for the Project monad
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

TODO
-}
module Ide3.Mechanism.Internal where

import Control.Monad.Trans.Except

import Ide3.Types 
import qualified Ide3.Project as Project 
import qualified Ide3.Module as Module 
import qualified Ide3.Import as Import 
import qualified Ide3.Export as Export 
import qualified Ide3.Declaration as Declaration

import Ide3.Monad

-- | Parse an import and add it to a module
addRawImport :: ProjectM m => ModuleInfo -> String -> ProjectResult m ImportId
addRawImport mi s = case Import.parse s of
    Right i -> addImport mi (WithBody i s)
    Left msg -> throwE $ "Failed to parse import: " ++ msg

-- | Parse an export and add it to a module
addRawExport :: ProjectM m => ModuleInfo -> String -> ProjectResult m  ExportId
addRawExport mi s = case Export.parse s of
    Right e -> addExport mi (WithBody e s)
    Left msg -> throwE $ "Failed to parse export: " ++ msg

-- | Parse a declaration and add it to a module
addRawDeclaration :: ProjectM m => ModuleInfo -> String -> ProjectResult m  ()
addRawDeclaration i s = case Declaration.parse s of
    Right d -> do addDeclaration i (WithBody d s)
                  return ()
    Left msg -> throwE $ "Failed to parse declaration: " ++ msg

-- | Parse an entire module and add it to the project
addRawModule :: ProjectM m => String -> Maybe FilePath -> ProjectResult m  ModuleInfo
addRawModule s p = case Module.parse s p of
    Right (m,eids,iids) -> do addModule m
                              return $ Module.info m
    Left msg -> throwE $ "Failed to parse module: " ++ msg

-- | Get the symbols exported by a module
getExternalSymbols :: ProjectM m => ModuleInfo -> ProjectResult m  [Symbol]
getExternalSymbols m = getModule m >>= Module.exportedSymbols >>= return . map getChild

-- | Get the symbols availible at the top level of a module
getInternalSymbols :: ProjectM m => ModuleInfo -> ProjectResult m  [Symbol]
getInternalSymbols m = getModule m >>= Module.internalSymbols
