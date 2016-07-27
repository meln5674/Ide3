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



import {-# SOURCE #-} Ide3.Module 
    ( allSymbols
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
symbolsProvided :: SolutionM m 
                => ProjectInfo 
                -> Module 
                -> Export 
                -> SolutionResult m u [Symbol]
symbolsProvided pi m e = case e of
    SingleExport s -> do
        internalSyms <- internalSymbols pi m
        if s `elem` internalSyms
            then return [s]
            else throwE $ SymbolNotFound (moduleInfo m) s "Export.symbolsProvided"
    ModuleExport n
        | m `importsModule` n -> do
            subM <- getModule pi (ModuleInfo n)
            syms <- exportedSymbols pi subM
            return $ map getChild syms
        | m `infoMatches` ModuleInfo n -> return $ map getChild $ allSymbols m
        | otherwise -> throwE 
                     $ ModuleNotImported (moduleInfo m) (ModuleInfo n) "Export.symbolsProvided"
    
    AggregateExport s (Just ss) -> 
        if exportedSyms `areAll` (`elem` allSyms)
            then do
                tree <- map getChild <$> symbolTree pi m s 
                case find (not . (`elem` tree)) ss of
                        Just s' -> throwE $ NotSubSymbol s s' "Export.symbolsProvided"
                        Nothing -> return $ exportedSyms
            else throwE $ SymbolNotFound (info m) s "Export.symbolsProvided"
      where
        allSyms = map getChild $ allSymbols m
        areAll = flip all
        exportedSyms = s:ss
    (AggregateExport s Nothing)
        | s `elem` map getChild (allSymbols m)
          -> map getChild <$> symbolTree pi m s
        | otherwise
          -> throwE $ SymbolNotFound (info m) s "Export.symbolsProvided"
