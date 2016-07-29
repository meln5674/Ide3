{-|
Module      : Ide3.Import
Description : Import declarations
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides operations on import statements
-}
module Ide3.Import
    ( module Ide3.Import
    , module Ide3.Import.Parser
    ) where

import Ide3.Types

import Data.List
import Data.Maybe

import Control.Monad.Trans.Except

import Ide3.Module.Common (EitherModule)
import qualified Ide3.Module.Common as Module
import {-# SOURCE #-} qualified Ide3.Module.Common.Query as Module

import Ide3.Monad
import Ide3.Import.Parser

import {-# SOURCE #-} Ide3.Mechanism.Internal

-- | Get the name of the module being imported, pre-rename
moduleName :: Import -> Symbol
moduleName (ModuleImport sym _ _) = sym
moduleName (WhitelistImport sym _ _ _) = sym
moduleName (BlacklistImport sym _ _ _) = sym

-- | Get if the import is qualified
isQualified :: Import -> Bool
isQualified (ModuleImport _ q _) = q
isQualified (WhitelistImport _ q _ _) = q
isQualified (BlacklistImport _ q _ _) = q

-- | Get if the import is renaming the module
renamed :: Import -> Maybe Symbol
renamed (ModuleImport _ _ r) = r
renamed (WhitelistImport _ _ r _) = r
renamed (BlacklistImport _ _ r _) = r

-- | Qualify a list of symbols
qualifySymbols :: Symbol        -- ^ Module name being imported as
               -> Bool          -- ^ Is the import qualified?
               -> [Symbol]      -- ^ List of symbols being imported
               -> [Symbol]      -- ^ List of symbols provided
qualifySymbols modName True syms = modName : map (modName `joinSym`) syms
qualifySymbols modName False syms = modName : syms

-- | Get the name of the module being imported, post-rename
importedModuleName :: Import -> Symbol
importedModuleName i = fromMaybe name rename
  where
    name = moduleName i
    rename = renamed i        

-- | Apply a transformation to the name of the module being imported
editModuleName :: (Symbol -> Symbol) -> (WithBody Import) -> (WithBody Import)
editModuleName f (WithBody (ModuleImport sym a b) s) = WithBody (ModuleImport (f sym) a b) $ error "TODO"
editModuleName f (WithBody (WhitelistImport sym a b c) s) = WithBody (WhitelistImport (f sym) a b c) $ error "TODO"
editModuleName f (WithBody (BlacklistImport sym a b c) s) = WithBody (BlacklistImport (f sym) a b c) $ error "TODO"

-- | Find the symbols to import from a module using a whitelist import
whitelistTree :: SolutionM m
              => ProjectInfo
              -> EitherModule  -- ^ Module symbols are being imported from
              -> ImportKind     -- ^ Specific import to search for
              -> SolutionResult m u [Symbol]
whitelistTree pi m i = do
    exSyms <- map getChild <$> Module.exportedSymbols pi m
    case i of
        NameImport s
            | s `elem` exSyms -> return [s]
            | otherwise -> throwE 
                         $ SymbolNotExported 
                            (Module.info m)
                            s 
                            "Import.whitelistTree"
        AbsImport _ _ -> error "FOUND AN ABS IMPORT"
        AggregateImport s Nothing -> map getChild <$> Module.symbolTree pi m s
        AggregateImport s (Just ss) -> do
            ls <- Module.symbolTree pi m s
            let ls' = map getChild ls
            case find (not . (`elem` ls')) ss of
                Just s' -> throwE $ NotSubSymbol s s' "Import.whitelistTree"
                Nothing -> return (s:ss)

-- | Find the symbosl to import from a module using a blacklist import
blacklistTree :: SolutionM m 
              => ProjectInfo
              -> EitherModule   -- ^ Module symbols are being imported from
              -> ImportKind     -- ^ Import to blacklist
              -> SolutionResult m u [Symbol]
blacklistTree pi m i = do
    whitelistSyms <- whitelistTree pi m i
    allSyms <- map getChild <$> Module.exportedSymbols pi m
    return $ filter (not . (`elem` whitelistSyms)) allSyms

-- | Get the symbols provided by an import, ignoring qualification
unqualSymbolsProvided :: SolutionM m => ProjectInfo -> Import -> SolutionResult m u [Symbol]
unqualSymbolsProvided pi i = case i of
    (ModuleImport sym _ _) -> getExternalSymbols pi (ModuleInfo sym)
    (WhitelistImport sym _ _ specs) -> do
        module_ <- getAnyModule pi (ModuleInfo sym)
        symbolsFromEach <- mapM (whitelistTree pi module_) specs
        return $ concat symbolsFromEach
    (BlacklistImport sym _ _ specs) -> do
        module_ <- getAnyModule pi (ModuleInfo sym)
        symbolsFromEach <- mapM (blacklistTree pi module_) specs
        return $ concat symbolsFromEach

-- | Get the symbols provided by an import
symbolsProvided :: SolutionM m => ProjectInfo -> Import -> SolutionResult m u [Symbol]
symbolsProvided pi i = qualifySymbols
    qualification
    shouldQualify
    <$> unqualSymbolsProvided pi i
  where
    qualification = importedModuleName i
    shouldQualify = isQualified i

-- | Test if an import provides a symbol
providesSymbol :: SolutionM m => ProjectInfo -> Import -> Symbol -> SolutionResult m u Bool
providesSymbol pi i s = do
    syms <- symbolsProvided pi i
    return $ s `elem` syms

-- | If this import provides a symbol, get all of the other symbols it provides
otherSymbols :: SolutionM m => ProjectInfo -> Import -> Symbol -> SolutionResult m u (Maybe [Symbol])
otherSymbols pi i s = do
    p <- providesSymbol pi i s
    if p
        then do
            syms <- symbolsProvided pi i
            return $ Just $ delete s syms
        else
            return Nothing

-- | Given a sub-symbol (class method, data constructor, etc...), find the other
-- sub-symbols and the parent symbol from this import
-- See 'Ide3.Module.symbolTree'
symbolTree :: SolutionM m => ProjectInfo -> Import -> Symbol -> SolutionResult m u [Symbol]
symbolTree pi i s = do
    module_ <- getAnyModule pi $ ModuleInfo $ moduleName i
    map getChild <$> Module.symbolTree pi module_ s
