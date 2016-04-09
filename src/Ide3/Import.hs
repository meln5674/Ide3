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

import Control.Monad.Trans.Except

import {-# SOURCE #-} Ide3.Module (exportedSymbols)
import {-# SOURCE #-} qualified Ide3.Module as Module

import Ide3.Monad
import Ide3.Import.Parser

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
importedModuleName i = case rename of
    Just name -> name
    Nothing -> name
  where
    name = moduleName i
    rename = renamed i

-- | Find the symbols to import from a module using a whitelist import
whitelistTree :: ProjectM m 
              => Module     -- ^ Module symbols are being imported from
              -> ImportKind -- ^ Specific import to search for
              -> ExceptT ProjectError m [Symbol]
whitelistTree m i = do
    exSyms <- map getChild <$> exportedSymbols m
    case i of
        NameImport s | s `elem` exSyms -> return [s]
                     | otherwise -> throwE $ "Import.whitelistTree: " ++ (show s) ++ " is not exported by " ++ (show m)
        --AbsImport
        AllImport s -> map getChild <$> Module.symbolTree m s
        SomeImport s ss -> do
            ls <- Module.symbolTree m s
            let ls' = map getChild ls
            case find (not . (`elem` ls')) ss of
                Just s' -> throwE $ "Import.whitelistTree: " ++ (show s) ++ " is not a sub-symbol of " ++ (show s)
                Nothing -> return (s:ss)

-- | Find the symbosl to import from a module using a blacklist import
blacklistTree :: ProjectM m 
              => Module     -- ^ Module symbols are being imported from
              -> ImportKind -- ^ Import to blacklist
              -> ExceptT ProjectError m [Symbol]
blacklistTree m i = do
    whitelistSyms <- whitelistTree m i
    allSyms <- map getChild <$> exportedSymbols m
    ExceptT $ return $ Right $ filter (not . (`elem` whitelistSyms)) $ allSyms

-- | Get the symbols provided by an import, ignoring qualification
unqualSymbolsProvided :: ProjectM m => Import -> ExceptT ProjectError m [Symbol]
unqualSymbolsProvided m@(ModuleImport sym _ _) = do
    mod <- getModule (ModuleInfo sym)
    moduleSyms <- exportedSymbols mod
    return $ map getChild moduleSyms
unqualSymbolsProvided m@(WhitelistImport sym _ _ specs) = do
    mod <- getModule (ModuleInfo sym)
    moduleSyms <- concat <$> mapM (whitelistTree mod) specs
    return moduleSyms
unqualSymbolsProvided m@(BlacklistImport sym _ _ specs) = do
    mod <- getModule (ModuleInfo sym)
    moduleSyms <- concat <$> mapM (blacklistTree mod) specs
    return moduleSyms

-- | Get the symbols provided by an import
symbolsProvided :: ProjectM m => Import -> ExceptT ProjectError m [Symbol]
symbolsProvided i = qualifySymbols (importedModuleName i)
                                     (isQualified i) 
                                 <$> (unqualSymbolsProvided i)

-- | Test if an import provides a symbol
providesSymbol :: ProjectM m => Import -> Symbol -> ExceptT ProjectError m Bool
providesSymbol i s = do
    syms <- symbolsProvided i
    return $ s `elem` syms

-- | If this import provides a symbol, get all of the other symbols it provides
otherSymbols :: ProjectM m => Import -> Symbol -> ExceptT ProjectError m (Maybe [Symbol])
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
symbolTree :: ProjectM m => Import -> Symbol -> ExceptT ProjectError m [Symbol]
symbolTree i s = do
    mod <- getModule (ModuleInfo (moduleName i))
    map getChild <$> Module.symbolTree mod s
