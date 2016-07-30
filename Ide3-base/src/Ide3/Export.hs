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

import Control.Monad.Trans.Except


import qualified Ide3.Module as Module
import {-# SOURCE #-} qualified Ide3.Module.Query as Module

import Ide3.Monad
import Ide3.Export.Parser

-- | Get a list of the symbols this export provides
symbolsProvided :: SolutionM m 
                => ProjectInfo 
                -> Module 
                -> Export 
                -> SolutionResult m u [Symbol]
symbolsProvided pji m e = case e of
    SingleExport s -> do
        internalSyms <- Module.internalSymbols pji m
        if s `elem` internalSyms
            then return [s]
            else throwE $ SymbolNotFound (Module.info m) s "Export.symbolsProvided"
    ModuleExport n
        | m `Module.importsModule` n -> do
            subM <- getModule pji (ModuleInfo n)
            syms <- Module.exportedSymbols pji subM
            return $ map getChild syms
        | m `Module.infoMatches` ModuleInfo n -> return $ map getChild $ Module.allSymbols m
        | otherwise -> throwE 
                     $ ModuleNotImported pji (moduleInfo m) (ModuleInfo n) "Export.symbolsProvided"
    
    AggregateExport s (Just ss) -> 
        if exportedSyms `areAll` (`elem` allSyms)
            then do
                tree <- map getChild <$> Module.symbolTree pji m s 
                case find (not . (`elem` tree)) ss of
                        Just s' -> throwE $ NotSubSymbol s s' "Export.symbolsProvided"
                        Nothing -> return exportedSyms
            else throwE $ SymbolNotFound (Module.info m) s "Export.symbolsProvided"
      where
        allSyms = map getChild $ Module.allSymbols m
        areAll = flip all
        exportedSyms = s:ss
    (AggregateExport s Nothing)
        | s `elem` map getChild (Module.allSymbols m)
          -> map getChild <$> Module.symbolTree pji m s
        | otherwise
          -> throwE $ SymbolNotFound (Module.info m) s "Export.symbolsProvided"
