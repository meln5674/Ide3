module Ide3.Export where

import Data.List (find)

import Ide3.Types

import Control.Monad.Trans.Except

import Language.Haskell.Exts.Parser (ParseResult(..))
import Language.Haskell.Exts.Annotated.Parser hiding (parse)
import qualified Language.Haskell.Exts.Annotated.Parser as Parser
import Language.Haskell.Exts.Annotated.Syntax hiding (Module)
import Language.Haskell.Exts.SrcLoc

import {-# SOURCE #-} Ide3.Module ( allSymbols
                                  , exportedSymbols
                                  , importsModule
                                  , symbolTree
                                  )

import Ide3.Monad
import Ide3.SrcLoc

convert :: ExportSpec a -> Export
convert export = case export of
    EVar _ n -> SingleExport (toSym n)
    EAbs _ (NoNamespace _) n -> SingleExport (toSym n)
    --EAbs ?? n
    EThingAll _ n -> AggregateExport (toSym n) Nothing
    EThingWith _ n ns -> AggregateExport (toSym n) (Just $ map toSym ns)
    EModuleContents _ n -> ModuleExport (toSym n)

convertWithBody :: Spanable a => String -> ExportSpec a -> WithBody Export
convertWithBody str export = WithBody export' body 
  where
    export' = convert export
    body = ann export >< str

parse :: String -> Either String Export
parse s = case result of
    ParseOk ok -> Right $ convert export
      where 
        headAndImports = unNonGreedy ok
        ModuleHeadAndImports _ _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ exportList)))) _ = headAndImports
        [export] = exportList
    ParseFailed _ s -> Left s
  where
    dummyHeader = "module DUMMY (" ++ s ++ ") where"
    result = Parser.parse dummyHeader :: (ParseResult (NonGreedy (ModuleHeadAndImports SrcSpanInfo)))

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
