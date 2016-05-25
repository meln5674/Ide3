{-|
Module      : Ide3.Export
Description : Export statements
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides operation on export statements
-}
module Ide3.Export 
    ( module Ide3.Export
    , module Ide3.Export.Parser
    ) where

import Data.List (find)

import Ide3.Types

import Control.Monad
import Control.Monad.Trans.Except



import {-# SOURCE #-} Ide3.Module ( allSymbols
                                  , exportedSymbols
                                  , importsModule
                                  , symbolTree
                                  , infoMatches
                                  , internalSymbols
                                  , info
                                  )

import Ide3.Monad
import Ide3.Export.Parser

-- | Get a list of the symbols this export provides
symbolsProvided :: ProjectM m => Module -> Export -> ProjectResult m u [Symbol]
symbolsProvided m@(Module i _ _ _ _) (SingleExport s) = do
    p <- liftM (s `elem`) $ internalSymbols m
    if p
        then return [s]
        else throwE $ SymbolNotFound i s "Export.symbolsProvided"
symbolsProvided m@(Module i _ _ _ _) (ModuleExport n)
    | m `importsModule` n
      = liftM (map getChild) $ getModule (ModuleInfo n) >>= exportedSymbols 
    | m `infoMatches` ModuleInfo n = return $ map getChild $ allSymbols m
    | otherwise
      = throwE $ ModuleNotImported i (ModuleInfo n) "Export.symbolsProvided"
symbolsProvided m (AggregateExport s (Just ss))
    | (`elem` (map getChild $ allSymbols m)) `all` (s:ss)
      = do
        tree <- map getChild <$> symbolTree m s 
        case find (not . (`elem` tree)) ss of
                Just s' -> throwE $ NotSubSymbol s s' "Export.symbolsProvided"
                Nothing -> return $ s:ss
    | otherwise
      = throwE $ SymbolNotFound (info m) s "Export.symbolsProvided"
symbolsProvided m (AggregateExport s Nothing)
    | s `elem` map getChild (allSymbols m)
      = map getChild <$> symbolTree m s
    | otherwise
      = throwE $ SymbolNotFound (info m) s "Export.symbolsProvided"
