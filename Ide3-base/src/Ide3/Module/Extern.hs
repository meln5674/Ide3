{-|
Module      : Ide3.Module.Extern
Description : External modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Module.Extern 
    ( module Ide3.Module.Extern
    , info
    , new
    ) where

import qualified Data.Map as Map

import Ide3.Types.Internal

import Ide3.Module.Extern.Internal

-- | Get the symbols exported by an external module
exportSymbols :: ExternExport -> [Symbol]
exportSymbols (SingleExternExport s) = [s]
exportSymbols (MultiExternExport s ss) = s : ss

-- | Get the symbols exported by an external module, annotated with that modules
-- identifying information
exportedSymbols :: ExternModule -> [ModuleChild Symbol]
exportedSymbols m = do
    syms <- map exportSymbols $ Map.elems $ externModuleExports m
    map (ModuleChild $ info m) syms
