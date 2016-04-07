module Ide3.Import where

import Ide3.Types

import Data.List

import Control.Monad.Trans.Except

import Language.Haskell.Exts.Annotated.Parser
import Language.Haskell.Exts.Parser (ParseResult(..))
import Language.Haskell.Exts.Annotated.Syntax hiding (Symbol, Module)
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import Language.Haskell.Exts.SrcLoc

import {-# SOURCE #-} Ide3.Module (exportedSymbols)
import {-# SOURCE #-} qualified Ide3.Module as Module

import Ide3.Monad
import Ide3.SrcLoc

convert :: Show a => ImportDecl a -> Import
convert x = case importSpecs x of
    Nothing -> ModuleImport sym isQualified rename
    Just (ImportSpecList _ True ss) -> BlacklistImport sym isQualified rename (map getSpec ss)
    Just (ImportSpecList _ False ss) -> WhitelistImport sym isQualified rename (map getSpec ss)
  where
    ModuleName _ n = importModule x
    sym = Symbol n
    rename = case importAs x of
        Just (ModuleName _ n) -> Just (Symbol n)
        Nothing -> Nothing
    isQualified = importQualified x

convertWithBody :: (Spanable a,Show a) => String -> ImportDecl a -> WithBody Import
convertWithBody str x = WithBody import_ body
  where
    body = ann x >< str
    import_ = convert x

parse :: String -> Either String Import
parse s = case parseImportDecl s of
    ParseOk x -> Right $ convert x
    ParseFailed _ s -> Left s

getSpec :: Show a => ImportSpec a -> ImportKind
getSpec (IVar _ n) = NameImport (toSym n)
getSpec (IAbs _ (NoNamespace _) n) = NameImport (toSym n)
getSpec (IThingAll _ n) = AllImport (toSym n)
getSpec (IThingWith _ n ns) = SomeImport (toSym n) (map toSym ns)
getSpec x = error $ show x

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

providesSymbol :: ProjectM m => Import -> Symbol -> ExceptT ProjectError m Bool
providesSymbol i s = do
    syms <- symbolsProvided i
    return $ s `elem` syms

otherSymbols :: ProjectM m => Import -> Symbol -> ExceptT ProjectError m (Maybe [Symbol])
otherSymbols i s = do
    p <- i `providesSymbol` s
    if p
        then do
            syms <- symbolsProvided i
            return $ Just $ delete s syms
        else
            return Nothing

symbolTree :: ProjectM m => Import -> Symbol -> ExceptT ProjectError m [Symbol]
symbolTree i s = do
    mod <- ExceptT $ getModule (ModuleInfo (moduleName i))
    map getChild <$> Module.symbolTree mod s
