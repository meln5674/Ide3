module Ide3.Import.Internal where

import Ide3.Types

import Data.List
import Data.Maybe

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
editModuleName :: (Symbol -> Symbol) 
               -> WithBody Import
               -> WithBody Import
editModuleName f (WithBody (ModuleImport sym a b) s)
    = WithBody (ModuleImport (f sym) a b) $ error "TODO"
editModuleName f (WithBody (WhitelistImport sym a b c) s)
    = WithBody (WhitelistImport (f sym) a b c) $ error "TODO"
editModuleName f (WithBody (BlacklistImport sym a b c) s)
    = WithBody (BlacklistImport (f sym) a b c) $ error "TODO"

