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



import {-# SOURCE #-} Ide3.Module ( allSymbols
                                  , exportedSymbols
                                  , importsModule
                                  , symbolTree
                                  )

import Ide3.Monad
import Ide3.Export.Parser

-- | Get a list of the symbols this export provides
symbolsProvided :: ProjectM m => Module -> Export -> ExceptT ProjectError m [Symbol]
symbolsProvided m (SingleExport s)
    | s `elem` (map getChild $ allSymbols m)
      = return [s]
    | otherwise                             
      = throwE $ "Export.symbolsProvided: " ++ (show s) ++ " is not an availible symbol in " ++ (show m)
symbolsProvided m (ModuleExport n)
    | m `importsModule` n
      = (ExceptT $ getModule $ ModuleInfo n) 
          >>= exportedSymbols 
          >>= return . map getChild
    | otherwise
      = throwE $ "Export.symbolsProvided: " ++ (show n) ++ " is not imported by " ++ (show m)
symbolsProvided m (AggregateExport s (Just ss))
    | (`elem` (map getChild $ allSymbols m)) `all` (s:ss)
      = do
        tree <- map getChild <$> symbolTree m s 
        case find (not . (`elem` tree)) ss of
                Just s' -> throwE $ "Export.symbolsProvided: " ++ (show s') ++ " is not a sub-symbol of " ++ (show m)
                Nothing -> return ss
    | otherwise
      = throwE $ "Export.symbolsProvided: " ++ (show s) ++ " is not an availible symbol in " ++ (show m)
symbolsProvided m (AggregateExport s Nothing)
    | s `elem` (map getChild $ allSymbols m)
      = map getChild <$> symbolTree m s
    | otherwise
      = throwE $ "Export.symbolsProvided: " ++ (show s) ++ " is not an availible symbol in " ++ (show m)
