module Ide3.Import where

import Ide3.Types

import Data.List (find)

import Control.Monad.Trans.Except

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax hiding (Symbol, Module)
import qualified Language.Haskell.Exts.Syntax as Syntax

import {-# SOURCE #-} Ide3.Module (exportedSymbols)
import {-# SOURCE #-} qualified Ide3.Module as Module

import Ide3.Monad



parse :: String -> Either String Import
parse s = case parseImportDecl s of
    ParseOk x -> case importSpecs x of
            Nothing -> Right $ ModuleImport sym isQualified rename
            Just (True, ss) -> Right $ BlacklistImport sym isQualified rename (map getSpec ss)
            Just (False, ss) -> Right $ WhitelistImport sym isQualified rename (map getSpec ss)
        where
            ModuleName n = importModule x
            sym = Symbol n
            rename = case importAs x of
                Just (ModuleName n) -> Just (Symbol n)
                Nothing -> Nothing
            isQualified = importQualified x
    ParseFailed _ s -> Left s

getSpec :: ImportSpec -> ImportKind
getSpec (IVar n) = NameImport (toSym n)
getSpec (IThingAll n) = AllImport (toSym n)
getSpec (IThingWith n ns) = SomeImport (toSym n) (map toSym ns)

moduleName :: Import -> Symbol
moduleName (ModuleImport sym _ _) = sym
moduleName (WhitelistImport sym _ _ _) = sym
moduleName (BlacklistImport sym _ _ _) = sym


isQualified :: Import -> Bool
isQualified (ModuleImport _ q _) = q
isQualified (WhitelistImport _ q _ _) = q
isQualified (BlacklistImport _ q _ _) = q

renamed :: Import -> Maybe Symbol
renamed (ModuleImport _ _ r) = r
renamed (WhitelistImport _ _ r _) = r
renamed (BlacklistImport _ _ r _) = r

symbolsProvided' :: Symbol -> Bool -> [Symbol] -> [Symbol]
symbolsProvided' modName True syms = modName : map (modName `joinSym`) syms
symbolsProvided' modName False syms = modName : syms

importedModuleName :: Import -> Symbol
importedModuleName i = case rename of
    Just name -> name
    Nothing -> name
  where
    name = moduleName i
    rename = renamed i

whitelistTree :: ProjectM m => Module -> ImportKind -> ExceptT ProjectError m [Symbol]
whitelistTree m i = do
    exSyms <- map getChild <$> exportedSymbols m
    case i of
        NameImport s | s `elem` exSyms -> return [s]
                     | otherwise -> throwE $ "Import.whitelistTree: " ++ (show s) ++ " is not exported by " ++ (show m)
          --where
            --s = getChild s'
        --AbsImport
        AllImport s -> map getChild <$> Module.symbolTree m s
        SomeImport s ss -> do
            ls <- Module.symbolTree m s
            let ls' = map getChild ls
            case find (not . (`elem` ls')) ss of
                Just s' -> throwE $ "Import.whitelistTree: " ++ (show s) ++ " is not a sub-symbol of " ++ (show s)
                Nothing -> return (s:ss)
blacklistTree :: ProjectM m => Module -> ImportKind -> ExceptT ProjectError m [Symbol]
blacklistTree m i = do
    whitelistSyms <- whitelistTree m i
    allSyms <- map getChild <$> exportedSymbols m
    ExceptT $ return $ Right $ filter (not . (`elem` whitelistSyms)) $ allSyms

unqualSymbolsProvided :: ProjectM m => Import -> ExceptT ProjectError m [Symbol]
unqualSymbolsProvided m@(ModuleImport sym _ _) = do
    mod <- ExceptT $ getModule (ModuleInfo sym)
    moduleSyms <- exportedSymbols mod
    return $ map getChild moduleSyms
unqualSymbolsProvided m@(WhitelistImport sym _ _ specs) = do
    mod <- ExceptT $ getModule (ModuleInfo sym)
    moduleSyms <- concat <$> mapM (whitelistTree mod) specs
    return moduleSyms
unqualSymbolsProvided m@(BlacklistImport sym _ _ specs) = do
    mod <- ExceptT $ getModule (ModuleInfo sym)
    moduleSyms <- concat <$> mapM (blacklistTree mod) specs
    return moduleSyms

symbolsProvided :: ProjectM m => Import -> ExceptT ProjectError m [Symbol]
symbolsProvided i = symbolsProvided' (importedModuleName i)
                                     (isQualified i) 
                                 <$> (unqualSymbolsProvided i)

symbolTree :: ProjectM m => Import -> Symbol -> ExceptT ProjectError m [Symbol]
symbolTree i s = do
    mod <- ExceptT $ getModule (ModuleInfo (moduleName i))
    map getChild <$> Module.symbolTree mod s
