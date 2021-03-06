{-|
Module      : Ide3.Import.Internal
Description : Operations on import declarations
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
module Ide3.Import.Internal
    ( moduleName
    , isQualified
    , renamed
    , qualifySymbols
    , importedModuleName
    , commonPath
    , editModuleName
    ) where

import Data.Maybe
import Data.List

import qualified Data.Text as T

import Ide3.Types.Internal

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

-- | Test if two imports have a common module path, i.e. Data.List and Data.Map
commonPath :: Import -> Import -> Bool
commonPath i1 i2 = sameCount /= 0
  where
    m1 = getSymbol $ moduleName i1
    m2 = getSymbol $ moduleName i2
    p1 = T.splitOn "." m1
    p2 = T.splitOn "." m2
    inits1 = tail $ inits p1
    inits2 = tail $ inits p2
    sameCount = length $ takeWhile id $ zipWith (==) inits1 inits2

-- | Apply a transformation to the name of the module being imported
editModuleName :: (Symbol -> Symbol) 
               -> WithBody Import
               -> WithBody Import
editModuleName f (WithBody (ModuleImport sym a b) s)
    = WithBody (ModuleImport (f sym) a b) 
    $ T.replace (getSymbol sym) (getSymbol $ f sym) s
editModuleName f (WithBody (WhitelistImport sym a b c) s)
    = WithBody (WhitelistImport (f sym) a b c) 
    $ T.replace (getSymbol sym) (getSymbol $ f sym) s
editModuleName f (WithBody (BlacklistImport sym a b c) s)
    = WithBody (BlacklistImport (f sym) a b c) 
    $ T.replace (getSymbol sym) (getSymbol $ f sym) s
