module Ide3.Module.Extern where

import Control.Monad.Trans.Except

import Data.Monoid
import Data.List

import Ide3.Monad
import Ide3.Types

exportSymbols :: ExternExport -> [Symbol]
exportSymbols (SingleExternExport s) = [s]
exportSymbols (MultiExternExport s ss) = s : ss

exportedSymbols :: ExternModule -> [ModuleChild Symbol]
exportedSymbols (ExternModule i es) = do
    syms <- map exportSymbols es
    map (ModuleChild i) syms

symbolTree :: ProjectM m => ExternModule -> Symbol -> ProjectResult m [ModuleChild Symbol]
symbolTree (ExternModule i es) s = case getFirst $ mconcat $ map look es of
    Just ss -> return $ map (ModuleChild i) ss
    Nothing -> throwE $ "Extern.symbolTree: " ++ show s ++ " is not provided by " ++ show i
  where
    look (SingleExternExport s') | s == s' = First $ Just []
    look (MultiExternExport s' ss) | s' `elem` ss = First $ Just $ delete s' ss
    look _ = First Nothing
