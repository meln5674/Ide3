{-|
Module      : Ide3.Module.Extern
Description : External modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Module.Extern where

import Control.Monad.Trans.Except

import Data.Monoid
import Data.List

import Ide3.Monad
import Ide3.Types

-- | Get the identifying information from an external module
info :: ExternModule -> ModuleInfo
info (ExternModule i _) = i

-- | Get the symbols exported by an external module
exportSymbols :: ExternExport -> [Symbol]
exportSymbols (SingleExternExport s) = [s]
exportSymbols (MultiExternExport s ss) = s : ss

-- | Get the symbols exported by an external module, annotated with that modules
-- identifying information
exportedSymbols :: ExternModule -> [ModuleChild Symbol]
exportedSymbols (ExternModule i es) = do
    syms <- map exportSymbols es
    map (ModuleChild i) syms

-- | Given a symbol such as a class method of data constructor, find the rest
-- of the related symbols exported by an external module.
-- The result list's head is the data type/class, and the tail as the other
-- symbols
-- This does not return the symbol provided
symbolTree :: SolutionM m => ExternModule -> Symbol -> SolutionResult m u [ModuleChild Symbol]
symbolTree (ExternModule i es) s = case getFirst $ mconcat $ map look es of
    Just ss -> return $ map (ModuleChild i) ss
    Nothing -> throwE $ SymbolNotExported i s "Extern.symbolTree"
  where
    look (SingleExternExport s') | s == s' = First $ Just []
    look (MultiExternExport s' ss) | s' `elem` ss = First $ Just $ delete s' ss
    look _ = First Nothing
