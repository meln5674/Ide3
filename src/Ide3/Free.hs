{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Ide3.Free where

import Control.Monad
import Control.Monad.Free

import Ide3.Types

data ProjectAST 
        projectNew projectLoad projectSave
        next 
    = CreateProject projectNew next
    | LoadProject projectLoad next
    | SaveProject projectSave next
    
    | CreateModule ModuleInfo next
    | RemoveModule ModuleInfo next
    
    | CreateDeclaration ModuleInfo String ((DeclarationInfo, Declaration) -> next)
    | EditDeclaration ModuleInfo DeclarationInfo 
        ((DeclarationInfo,Declaration,String) -> (DeclarationInfo,Declaration,String)) next
    | RemoveDeclaration ModuleInfo DeclarationInfo next
    
    | AddImport ModuleInfo String ((ImportId,Import) -> next)
    | EditImport ModuleInfo ImportId 
        ((ImportId,Import,String) -> (ImportId,Import,String)) next
    | RemoveImport ModuleInfo ImportId next
    
    | AddExport ModuleInfo String ((ExportId,Export) -> next)
    | EditExport ModuleInfo ExportId 
        ((ExportId,Export,String) -> (ExportId,Export,String)) next
    | RemoveExport ModuleInfo ExportId next
    | ExportAll ModuleInfo next
    
    | GetModules ([ModuleInfo] -> next)
    | GetDeclarations ModuleInfo ([DeclarationInfo] -> next)
    | GetImports ModuleInfo ([ImportId] -> next)
    | GetExports ModuleInfo (Maybe [ExportId] -> next)
    
    | GetDeclaration ModuleInfo DeclarationInfo ((DeclarationInfo,Declaration,String) -> next)
    | GetImport ModuleInfo ImportId ((ImportId,Import,String) -> next)
    | GetExport ModuleInfo ExportId ((ExportId,Export,String) -> next)
    deriving (Functor)

{-
instance Functor (ProjectAST b) where
    fmap f (AddModule mi next) = AddModule mi (f next)
    fmap f (RemoveModule mi next) = RemoveModule mi (f next)
    fmap f (AddDeclaration mi di d next) = AddDeclaration mi di d (f next)
-}

  
newtype ProjectMonad 
        projectNew projectLoad projectSave
        r
    = MkProjectMonad
        { runProjectMonad :: Free 
            ( ProjectAST
                projectNew projectLoad projectSave
            ) r
        }
    deriving (Functor, Applicative, Monad)


{-
addModule :: ModuleInfo -> ProjectMonad () ()
addModule mi = MkProjectMonad $ liftF (AddModule mi ())
removeModule :: ModuleInfo -> ProjectMonad () ()
removeModule mi = MkProjectMonad $ liftF (RemoveModule mi ())
addDeclaration:: ModuleInfo -> DeclarationInfo -> Declaration -> ProjectMonad () ()
addDeclaration mi di d = MkProjectMonad $ liftF (AddDeclaration mi di d ())
-}

createProject :: projectNew -> ProjectMonad projectNew projectLoad projectSave ()
createProject pn = MkProjectMonad $ liftF $ CreateProject pn ()

loadProject :: projectLoad -> ProjectMonad projectNew projectLoad projectSave ()
loadProject pl = MkProjectMonad $ liftF $ LoadProject pl ()

saveProject :: projectSave -> ProjectMonad projectNew projectLoad projectSave ()
saveProject ps = MkProjectMonad $ liftF $ SaveProject ps ()


createModule :: ModuleInfo -> ProjectMonad projectNew projectLoad projectSave ()
createModule mk = MkProjectMonad $ liftF $ CreateModule mk () 

removeModule :: ModuleInfo -> ProjectMonad projectNew projectLoad projectSave ()
removeModule mk = MkProjectMonad $ liftF $ RemoveModule mk ()

{-
createExternModule mk = MkProjectMonad $ liftF $ CreateExternModule mk ()
removeExternModule mk = MkProjectMonad $ liftF $ RemoveExternModule mk ()
-}

createDeclaration :: ModuleInfo 
                  -> String 
                  -> ProjectMonad projectNew projectLoad projectSave (DeclarationInfo, Declaration)
createDeclaration mk b = MkProjectMonad $ liftF $ CreateDeclaration mk b id
editDeclaration :: ModuleInfo
                -> DeclarationInfo
                -> ((DeclarationInfo, Declaration, String)
                    -> (DeclarationInfo, Declaration, String))
                -> ProjectMonad projectNew projectLoad projectSave ()
editDeclaration mk dk f = MkProjectMonad $ liftF $ EditDeclaration mk dk f ()
removeDeclaration :: ModuleInfo
                  -> DeclarationInfo
                  -> ProjectMonad projectNew projectLoad projectSave ()
removeDeclaration mk dk = MkProjectMonad $ liftF $ RemoveDeclaration mk dk ()

addImport :: ModuleInfo
                       -> String
                       -> ProjectMonad
                            projectNew projectLoad projectSave (ImportId, Import)
addImport mk b = MkProjectMonad $ liftF $ AddImport mk b id
editImport :: ModuleInfo
                -> ImportId
                -> ((ImportId, Import, String) -> (ImportId, Import, String))
                -> ProjectMonad projectNew projectLoad projectSave ()
editImport mk ik f = MkProjectMonad $ liftF $ EditImport mk ik f ()
removeImport mk ik = MkProjectMonad $ liftF $ RemoveImport mk ik ()

addExport mk b = MkProjectMonad $ liftF $ AddExport mk b id
editExport mk ek f = MkProjectMonad $ liftF $ EditExport mk ek f ()
removeExport mk ek = MkProjectMonad $ liftF $ RemoveExport mk ek ()
exportAll mk = MkProjectMonad $ liftF $ ExportAll mk ()

getModules = MkProjectMonad $ liftF $ GetModules id
getDeclarations mk = MkProjectMonad $ liftF $ GetDeclarations mk id
getImports mk = MkProjectMonad $ liftF $ GetImports mk id
getExports mk = MkProjectMonad $ liftF $ GetExports mk id

getDeclaration mk dk = MkProjectMonad $ liftF $ GetDeclaration mk dk id
getImport mk ik = MkProjectMonad $ liftF $ GetImport mk ik id
getExport mk ek = MkProjectMonad $ liftF $ GetExport mk ek id


snd3 (_,x,_) = x
trd (_,_,x) = x

getAllDeclarations = do
    mks <- getModules
    declPartitions <- forM mks getDeclarations
    return $ concat declPartitions

dumpModule mk = do
    dks <- getDeclarations mk 
    iks <- getImports mk
    eks <- getExports mk
    ds <- forM dks $ liftM trd . getDeclaration mk
    is <- forM iks $ liftM trd . getImport mk
    es <- case eks of
        Nothing -> return Nothing
        Just eks -> liftM Just $ forM eks $ liftM trd . getExport mk
    return (ds,is,es)

addModule mk ds is es = do
    createModule mk
    forM ds $ createDeclaration mk
    forM is $ addImport mk
    case es of
        Nothing -> exportAll mk
        Just es -> forM_ es $ addExport mk

renameModule mv mk = do
    (ds,is,es) <- dumpModule mk
    removeModule mk
    let mk' = mv mk
    addModule mk' ds is es

moveDeclaration mksrc mkdest dk = do
    db <- liftM trd $ getDeclaration mksrc dk
    removeDeclaration mksrc dk
    createDeclaration mkdest db

withDeclarations mk f = liftM f $ getDeclarations mk
withImports mk f = liftM f $ getImports mk
withExports mk f = liftM (liftM f) $ getExports mk

mapDeclarations mk f = withDeclarations mk $ map f
mapImports mk f = withImports mk $ map f
mapExports mk f = withExports mk $ map f

filterDeclarations mk f = withDeclarations mk $ filter f
filterImports mk f = withImports mk $ filter f
filterExports mk f = withExports mk $ filter f

hasDeclaration mk dk = withDeclarations mk $ any (== dk)
hasImport mk ik = withImports mk $ any (== ik)
hasExport mk ek = withExports mk $ any (== ek)

filterAllDeclarations f = do
    mks <- getModules
    liftM concat
        $ forM mks 
        $ \mk -> liftM (map $ \dk -> (mk,dk))
        $ filterDeclarations mk f
