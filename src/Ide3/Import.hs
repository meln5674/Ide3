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

import {-# SOURCE #-} Ide3.Module.Common (EitherModule)
import {-# SOURCE #-} qualified Ide3.Module.Common as Module

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

-- | Find the symbols to import from a module using a whitelist import
whitelistTree :: ProjectM m
              => EitherModule   -- ^ Module symbols are being imported from
              -> ImportKind     -- ^ Specific import to search for
              -> ProjectResult m u [Symbol]
whitelistTree m i = do
    exSyms <- map getChild <$> Module.exportedSymbols m
    case i of
        NameImport s | s `elem` exSyms -> return [s]
                     | otherwise -> throwE $ SymbolNotExported (Module.info m) s "Import.whitelistTree"
        AbsImport _ _ -> error "FOUND AN ABS IMPORT"
        AggregateImport s Nothing -> map getChild <$> Module.symbolTree m s
        AggregateImport s (Just ss) -> do
            ls <- Module.symbolTree m s
            let ls' = map getChild ls
            case find (not . (`elem` ls')) ss of
                Just s' -> throwE $ NotSubSymbol s s' "Import.whitelistTree"
                Nothing -> return (s:ss)

-- | Find the symbosl to import from a module using a blacklist import
blacklistTree :: ProjectM m 
              => EitherModule   -- ^ Module symbols are being imported from
              -> ImportKind     -- ^ Import to blacklist
              -> ProjectResult m u [Symbol]
blacklistTree m i = do
    whitelistSyms <- whitelistTree m i
    allSyms <- map getChild <$> Module.exportedSymbols m
--    ExceptT $ return $ Right $ filter (not . (`elem` whitelistSyms)) allSyms
    return $ filter (not . (`elem` whitelistSyms)) allSyms

-- | Get the symbols provided by an import, ignoring qualification
unqualSymbolsProvided :: ProjectM m => Import -> ProjectResult m u [Symbol]
unqualSymbolsProvided (ModuleImport sym _ _) = getExternalSymbols (ModuleInfo sym)
unqualSymbolsProvided (WhitelistImport sym _ _ specs) = do
    module_ <- getAnyModule (ModuleInfo sym)
    symbolsFromEach <- mapM (whitelistTree module_) specs
    return $ concat symbolsFromEach
unqualSymbolsProvided (BlacklistImport sym _ _ specs) = do
    module_ <- getAnyModule (ModuleInfo sym)
    symbolsFromEach <- mapM (blacklistTree module_) specs
    return $ concat symbolsFromEach

-- | Get the symbols provided by an import
symbolsProvided :: ProjectM m => Import -> ProjectResult m u [Symbol]
symbolsProvided i = qualifySymbols (importedModuleName i)
                                     (isQualified i) 
                                 <$> unqualSymbolsProvided i

-- | Test if an import provides a symbol
providesSymbol :: ProjectM m => Import -> Symbol -> ProjectResult m u Bool
providesSymbol i s = do
    syms <- symbolsProvided i
    return $ s `elem` syms

-- | If this import provides a symbol, get all of the other symbols it provides
otherSymbols :: ProjectM m => Import -> Symbol -> ProjectResult m u (Maybe [Symbol])
otherSymbols i s = do
    p <- i `providesSymbol` s
    if p
        then do
            syms <- symbolsProvided i
            return $ Just $ delete s syms
        else
            return Nothing

-- | Given a sub-symbol (class method, data constructor, etc...), find the other
-- sub-symbols and the parent symbol from this import
-- See 'Ide3.Module.symbolTree'
symbolTree :: ProjectM m => Import -> Symbol -> ProjectResult m u [Symbol]
symbolTree i s = do
    module_ <- getAnyModule (ModuleInfo (moduleName i))
    map getChild <$> Module.symbolTree module_ s
