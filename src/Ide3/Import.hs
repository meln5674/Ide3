module Ide3.Import where

import Ide3.Types

import Control.Monad.Trans.Maybe

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax hiding (Symbol, Module)
import qualified Language.Haskell.Exts.Syntax as Syntax

import {-# SOURCE #-} Ide3.Module (exportedSymbols)
import {-# SOURCE #-} qualified Ide3.Module as Module

import Ide3.Monad

class ToSym a where
    toSym :: a -> Symbol

instance ToSym Name where
    toSym (Ident n) = Symbol n
    toSym (Syntax.Symbol n) = Symbol n

instance ToSym CName where
    toSym (VarName n) = toSym n
    toSym (ConName n) = toSym n

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

whitelistTree :: ProjectM m => Module -> ImportKind -> MaybeT m [Symbol]
whitelistTree m i = do
    exSyms <- MaybeT $ Just <$> map getChild <$> exportedSymbols m
    case i of
        NameImport s | s `elem` exSyms -> return [s]
                     | otherwise -> MaybeT $ return Nothing
          --where
            --s = getChild s'
        --AbsImport
        AllImport s -> map getChild <$> Module.symbolTree m s
        SomeImport s ss -> do
            ls <- Module.symbolTree m s
            let ls' = map getChild ls
            case all (`elem` ls') ss of
                True -> return (s:ss)
                False -> MaybeT $ return Nothing
blacklistTree :: ProjectM m => Module -> ImportKind -> MaybeT m [Symbol]
blacklistTree m i = do
    whitelistSyms <- whitelistTree m i
    allSyms <- MaybeT $ Just <$> map getChild <$> exportedSymbols m
    MaybeT $ return $ Just <$> filter (not . (`elem` whitelistSyms)) $ allSyms

unqualSymbolsProvided :: ProjectM m => Import -> MaybeT m [Symbol]
unqualSymbolsProvided m@(ModuleImport sym _ _) = do
    mod <- MaybeT $ getModule (ModuleInfo sym)
    moduleSyms <- MaybeT $ Just <$> exportedSymbols mod
    return $ map getChild moduleSyms
unqualSymbolsProvided m@(WhitelistImport sym _ _ specs) = do
    mod <- MaybeT $ getModule (ModuleInfo sym)
    moduleSyms <- concat <$> mapM (whitelistTree mod) specs
    return moduleSyms
unqualSymbolsProvided m@(BlacklistImport sym _ _ specs) = do
    mod <- MaybeT $ getModule (ModuleInfo sym)
    moduleSyms <- concat <$> mapM (blacklistTree mod) specs
    return moduleSyms

symbolsProvided :: ProjectM m => Import -> MaybeT m [Symbol]
symbolsProvided i = symbolsProvided' (importedModuleName i)
                                     (isQualified i) 
                                 <$> (unqualSymbolsProvided i)

symbolTree :: ProjectM m => Import -> Symbol -> MaybeT m [Symbol]
symbolTree i s = do
    mod <- MaybeT $ getModule (ModuleInfo (moduleName i))
    map getChild <$> Module.symbolTree mod s
