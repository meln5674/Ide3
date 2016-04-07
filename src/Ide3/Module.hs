module Ide3.Module where

import Control.Monad

import Data.Monoid

import Data.List (intercalate, delete, find)

import Control.Monad.Trans.Except

import qualified Data.Map as Map
import Data.Map.Strict ( Map )

import Ide3.Types
import Ide3.Monad hiding (new, addExport, addImport, addDeclaration)
import qualified Ide3.Declaration as Declaration
import qualified Ide3.Export as Export
import qualified Ide3.Import as Import

import Ide3.Module.Parser (ExtractionResults(..))
import qualified Ide3.Module.Parser as Parser

info :: Module -> ModuleInfo
info (Module i _ _ _) = i

empty :: Module
empty = Module (ModuleInfo (Symbol "")) Map.empty Nothing Map.empty

new :: ModuleInfo -> Module
new i = Module i Map.empty Nothing Map.empty

foldlRes :: (b -> a -> (b,c)) -> b -> [a] -> (b,[c])
foldlRes f x [] = (x,[])
foldlRes f x (y:ys) =
    let (x',z) = f x y
        (x'', zs) = foldlRes f x' ys
    in (x'',z:zs)
                    

parse :: String -> Either ProjectError (Module,[ExportId],[ImportId])
parse s = case Parser.parse s of
    Right (Extracted info exports imports decls) -> Right $ (withDecls, eids, iids)
      where
        newModule = new info
        (withExports,eids) = foldlRes addExport newModule exports
        (withImports,iids) = foldlRes addImport withExports imports
        withDecls = foldl addDeclaration withImports decls
    Left msg -> Left msg

getImports :: Module -> [WithBody Import]
getImports (Module _ is _ _) = Map.elems is

getDeclarations :: Module -> [WithBody Declaration]
getDeclarations (Module _ _ _ ds) = Map.elems ds

getExports :: Module -> [WithBody Export]
getExports (Module _ _ Nothing _) = []
getExports (Module _ _ (Just es) _) = Map.elems es

{-
getExportTexts :: Module -> [String]
getExportTexts (Module _ _ es _) = case es of
    Nothing -> []
    Just es -> map body $ Map.elems es
-}
getHeaderText :: Module -> String
getHeaderText m = case bodies $ getExports m of
    [] -> "module " ++ name ++ " where"
    es -> "module " ++ name ++ "(" ++ intercalate "," es ++ ") where"
  where
    ModuleInfo (Symbol name) = info m
{-
getImportTexts :: Module -> [String]
getImportTexts (Module _ is _ _) = map body $ Map.elems is

getDeclarationTexts :: Module -> [String]
getDeclarationTexts (Module _ _ _ ds) = map body $ Map.elems ds
-}
toFile :: Module -> String
toFile m@(Module (ModuleInfo (Symbol name)) is es ds)
    = intercalate "\n" $ parts
  where
    header = getHeaderText m
    imports = bodies $ getImports m
    declarations = bodies $ getDeclarations m
    parts = header : imports ++ declarations

modifiersOf :: Symbol -> Module -> [ModuleChild DeclarationInfo]
modifiersOf s m@(Module _ _ _ ds)
  = map (qualify m . Declaration.info . item) 
  . Map.elems
  . Map.filter ((`Declaration.affectsSymbol` s) . item)
  $ ds


qualify :: Module -> a -> ModuleChild a
qualify (Module i _ _ _) x = ModuleChild i x


exportedSymbols :: ProjectM m => Module -> ExceptT ProjectError m [ModuleChild Symbol]
exportedSymbols m@(Module _ _ Nothing _) = return $ allSymbols m
exportedSymbols m@(Module n _ (Just es) _) = do
    syms <- concat <$> mapM (Export.symbolsProvided m . item) es
    let qualSyms = map (qualify m) syms
    --return $ getModuleName n : qualSyms
    return qualSyms

internalSymbols :: ProjectM m => Module -> ExceptT ProjectError m [Symbol]
internalSymbols m@(Module _ is _ ds) = do
    importSyms <- concat <$> mapM (Import.symbolsProvided . item) is
    return $ importSyms ++ concatMap (Declaration.symbolsProvided . item) ds

allSymbols :: Module -> [ModuleChild Symbol]
allSymbols m@(Module _ _ _ ds)
  = concatMap (map (qualify m) . Declaration.symbolsProvided . item) ds


search :: (a -> Maybe b) -> [a] -> Maybe (a,b)
search f xs = case find pred $ map toPair xs of
    Just (x,Just y) -> Just (x,y)
    _ -> Nothing
  where
    toPair x = (x,f x)
    pred (_,Just y) = True
    pred _ = False

searchM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe (a,b))
searchM f xs = do
    r <- find pred <$> mapM toPair xs 
    case r of
        Just (x,Just y) -> return $ Just (x,y)
        _ -> return Nothing
  where
    toPair x = do
        y <- f x
        return (x,y)
    pred (_,Just y) = True
    pred _ = False

symbolTree :: ProjectM m => Module -> Symbol -> ExceptT ProjectError m [ModuleChild Symbol]
symbolTree m sym = do
    let declarations = items $ getDeclarations m
        searchResult = search (`Declaration.otherSymbols`sym) declarations
    case searchResult of
        Just (_,syms) -> return $ map (qualify m) syms
        Nothing -> do
            let imports = items $ getImports m
            searchResult <- searchM (`Import.otherSymbols` sym) imports
            case searchResult of
                Just (i,_) -> do
                    otherSyms <- Import.symbolTree i sym
                    return $ map (qualify m) otherSyms
                Nothing -> throwE $ "Module.symbolTree: " ++ (show sym) ++ " is not an availible symbol in " ++ (show m)

allDeclarations :: Module -> [ModuleChild DeclarationInfo]
allDeclarations m@(Module _ _ _ ds)
  = map (qualify m . Declaration.info . item) . Map.elems $ ds

infoMatches :: Module -> ModuleInfo -> Bool
infoMatches (Module i _ _ _) i' = i == i'

nextImportId :: Module -> ImportId
nextImportId (Module _ is _ _) = 1 + maximum (-1 : (Map.keys is))

addImport :: Module -> WithBody Import -> (Module, ImportId)
addImport m@(Module mi is es ds) i = (Module mi (Map.insert id i is) es ds, id)
    where
        id = nextImportId m

removeImport :: Module -> ImportId -> Either ProjectError Module
removeImport (Module mi is es ds) i
    = case Map.lookup i is of
        Just _ -> Right $ Module mi (Map.delete i is) es ds
        Nothing -> Left $ "Module.removeImport: no such import id: " ++ show i

importsModule :: Module -> Symbol -> Bool
importsModule m sym = sym `elem` (map Import.moduleName $ items $ getImports m)

exportAll :: Module -> Module
exportAll (Module mi is _ ds) = Module mi is Nothing ds

nextExportId :: Module -> ExportId
nextExportId (Module _ _ Nothing _) = 0
nextExportId (Module _ _ (Just es) _) = 1 + (maximum $ -1 :(Map.keys es))

addExport :: Module -> WithBody Export -> (Module,ExportId)
addExport m@(Module mi is es ds) e = (Module mi is es' ds,nextId)
  where
    nextId = nextExportId m
    es' = case es of
        Nothing -> Just $ Map.singleton nextId e
        Just es -> Just $ Map.insert nextId e es

removeExport :: Module -> ExportId -> Either ProjectError Module
removeExport (Module mi is Nothing ds) e = Left $ "Module.removeExport: Can't remove an export from an export all"
removeExport (Module mi is (Just es) ds) e
    = Right $ Module mi is (Just $ e `Map.delete` es) ds

addDeclaration :: Module -> WithBody Declaration -> Module
addDeclaration (Module i is es ds) d = Module i is es ds'
  where
    di = Declaration.info . item $ d
    ds' = Map.insert di d ds
  

hasDeclarationInfo :: Module -> DeclarationInfo -> Bool
hasDeclarationInfo m di = case getDeclaration m di of
    Right _ -> True
    Left  _ -> False

removeDeclaration :: Module -> DeclarationInfo -> Either ProjectError Module
removeDeclaration m@(Module i is es ds) di
    | m `hasDeclarationInfo` di = Right $ Module i is es ds'
    | otherwise                 = Left $ "Module.removeDeclaration: " ++ show di ++ " is not a declaration in" ++ show m
  where
    ds' = Map.delete di ds

editDeclaration :: Module 
                -> DeclarationInfo
                -> (Declaration -> Either ProjectError Declaration)
                -> Either ProjectError Module
editDeclaration m@(Module i is es ds) di f = do
    (ModuleChild _ (WithBody d s)) <- getDeclaration m di
    d' <- WithBody <$> (f d) <*> pure s
    let ds' = Map.insert di d' ds
    return $ Module i is es ds'

editDeclaration' m d f = editDeclaration m d (return . f)

getDeclaration :: Module
               -> DeclarationInfo
               -> Either ProjectError (ModuleChild (WithBody Declaration))
getDeclaration m@(Module _ _ _ ds) di = case Map.lookup di ds of
        Just d -> Right $ qualify m d
        Nothing -> Left $ "Module.getDeclaration: " ++ show di ++ " is not a declaration in " ++ show m

getDeclaration' :: Module
                -> DeclarationInfo
                -> Either ProjectError (ModuleChild Declaration)
getDeclaration' m di = do
    (ModuleChild i (WithBody d s)) <- getDeclaration m di
    return $ ModuleChild i d
