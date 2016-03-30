module Ide3.Module where

import Data.List (intercalate, delete, find)

import Control.Monad.Trans.Maybe

import qualified Data.Map as Map
import Data.Map.Strict ( Map )

import Ide3.Types
import Ide3.Monad
import qualified Ide3.Declaration as Declaration
import qualified Ide3.Export as Export
import qualified Ide3.Import as Import

info :: Module -> ModuleInfo
info (Module i _ _ _) = i

empty :: Module
empty = Module (ModuleInfo (Symbol "")) [] Nothing Map.empty

new :: ModuleInfo -> Module
new i = Module i [] Nothing Map.empty

toFile :: Module -> String
toFile (Module (ModuleInfo (Symbol name)) is es ds)
    = intercalate "\n" $ parts
  where
    header = case es of
        Nothing -> "module " ++ name ++ " where"
        Just es -> "module " ++ name ++ exports
          where 
            exports = "(" ++ intercalate "," (map body es) ++ ") where"
    imports = map body is 
    declarations = map body (Map.elems ds)
    parts = header : imports ++ declarations

modifiersOf :: Symbol -> Module -> [ModuleChild DeclarationInfo]
modifiersOf s m@(Module _ _ _ ds)
  = map (qualify m . Declaration.info . item) 
  . Map.elems
  . Map.filter ((`Declaration.affectsSymbol` s) . item)
  $ ds


qualify :: Module -> a -> ModuleChild a
qualify (Module i _ _ _) x = ModuleChild i x


exportedSymbols :: ProjectM m => Module -> m [ModuleChild Symbol]
exportedSymbols m@(Module _ _ Nothing _) = return $ allSymbols m
exportedSymbols m@(Module n _ (Just es) _) = do
    syms <- concat <$> mapM (Export.symbolsProvided m . item) es
    let qualSyms = map (qualify m) syms
    --return $ getModuleName n : qualSyms
    return qualSyms

internalSymbols :: ProjectM m => Module -> MaybeT m [Symbol]
internalSymbols m@(Module _ is _ ds) = do
    importSyms <- concat <$> mapM (Import.symbolsProvided . item) is
    return $ importSyms ++ concatMap (Declaration.symbolsProvided . item) ds

allSymbols :: Module -> [ModuleChild Symbol]
allSymbols m@(Module _ _ _ ds)
  = concatMap (map (qualify m) . Declaration.symbolsProvided . item) ds


symbolTree :: ProjectM m => Module -> Symbol -> MaybeT m [ModuleChild Symbol]
symbolTree m@(Module _ is _ ds) s = do
    case find (\d -> s `elem` Declaration.symbolsProvided d) (map item $ Map.elems ds) of
        Just d -> return $ map (qualify m) $ delete s $ Declaration.symbolsProvided d
        Nothing -> do
            provided <- mapM (\i -> (,) i <$> Import.symbolsProvided i) $ map item is
            case find ((s `elem`) . snd) provided of
                Just (i,_) -> map (qualify m) <$> Import.symbolTree i s
                Nothing -> MaybeT $ return Nothing
            
--symbolTree = undefined

allDeclarations :: Module -> [ModuleChild DeclarationInfo]
allDeclarations m@(Module _ _ _ ds)
  = map (qualify m . Declaration.info . item) . Map.elems $ ds

infoMatches :: Module -> ModuleInfo -> Bool
infoMatches (Module i _ _ _) i' = i == i'

addImport :: Module -> WithBody Import -> Module
addImport (Module mi is es ds) i = Module mi (i:is) es ds

removeImport :: Module -> WithBody Import -> Maybe Module
removeImport (Module mi is es ds) i = Just $ Module mi (i `delete` is) es ds
-- TODO

exportAll :: Module -> Module
exportAll (Module mi is _ ds) = Module mi is Nothing ds

addExport :: Module -> WithBody Export -> Module
addExport (Module mi is Nothing ds) e = Module mi is (Just [e]) ds
addExport (Module mi is (Just es) ds) e = Module mi is (Just $ e:es) ds

removeExport :: Module -> WithBody Export -> Maybe Module
removeExport (Module mi is Nothing ds) e = Nothing
removeExport (Module mi is (Just es) ds) e
    = Just $ Module mi is (Just $ e `delete` es) ds

addDeclaration :: Module -> WithBody Declaration -> Module
addDeclaration (Module i is es ds) d = (Module i is es ds')
  where
    di = Declaration.info . item $ d
    ds' = Map.insert di d ds
  

hasDeclarationInfo :: Module -> DeclarationInfo -> Bool
hasDeclarationInfo m di = case getDeclaration m di of
    Just _ -> True
    Nothing -> False

removeDeclaration :: Module -> DeclarationInfo -> Maybe Module
removeDeclaration m@(Module i is es ds) di
    | m `hasDeclarationInfo` di = Just $ Module i is es ds'
    | otherwise                 = Nothing
  where
    ds' = Map.delete di ds

editDeclaration :: Module 
                -> DeclarationInfo
                -> (Declaration -> Maybe Declaration)
                -> Maybe Module
editDeclaration m@(Module i is es ds) di f = do
    (ModuleChild _ (WithBody d s)) <- getDeclaration m di
    d' <- WithBody <$> (f d) <*> pure s
    let ds' = Map.insert di d' ds
    return $ Module i is es ds'

editDeclaration' m d f = editDeclaration m d (return . f)

getDeclaration :: Module
               -> DeclarationInfo
               -> Maybe (ModuleChild (WithBody Declaration))
getDeclaration m@(Module _ _ _ ds) di = qualify m <$> Map.lookup di ds

getDeclaration' :: Module
                -> DeclarationInfo
                -> Maybe (ModuleChild Declaration)
getDeclaration' m di = do
    (ModuleChild i (WithBody d s)) <- getDeclaration m di
    return $ ModuleChild i d
