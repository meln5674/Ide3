module Ide3.Export where

import Data.List (find)

import Ide3.Types

import Control.Monad.Trans.Except

import Language.Haskell.Exts.Parser hiding (parse)
import qualified Language.Haskell.Exts.Parser as Parser
import Language.Haskell.Exts.Syntax hiding (Module)
--import Language.Haskell.Exts.Annotated.Syntax hiding (Module)

import {-# SOURCE #-} Ide3.Module ( allSymbols
                                  , exportedSymbols
                                  , importsModule
                                  , symbolTree
                                  )

import Ide3.Monad

parse :: String -> Either String Export
parse s = case result of
    ParseOk ok -> Right $ case export of
        EVar n -> SingleExport (toSym n)
        EAbs NoNamespace n -> SingleExport (toSym n)
        --EAbs ?? n
        EThingAll n -> AggregateExport (toSym n) Nothing
        EThingWith n ns -> AggregateExport (toSym n) (Just $ map toSym ns)
        EModuleContents n -> ModuleExport (toSym n)
      where 
        headAndImports = unNonGreedy ok
        ModuleHeadAndImports _ (_, _, Just exportList) _ = headAndImports
        [export] = exportList
    ParseFailed _ s -> Left s
  where
    dummyHeader = "module DUMMY (" ++ s ++ ") where"
    result = Parser.parse dummyHeader :: (ParseResult (NonGreedy (ModuleHeadAndImports)))

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
