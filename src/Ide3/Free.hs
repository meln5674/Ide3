{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Ide3.Free where

import Control.Monad
import Control.Monad.Free

--import Ide3.Types

data ProjectAST 
        projectNew projectLoad projectSave
        moduleKey
        declKey declVal
        importKey importVal
        exportKey exportVal
        bodyType
        next 
    = CreateProject projectNew next
    | LoadProject projectLoad next
    | SaveProject projectSave next
    
    | CreateModule moduleKey next
    | RemoveModule moduleKey next
    
    | CreateDeclaration moduleKey bodyType ((declKey, declVal) -> next)
    | EditDeclaration moduleKey declKey 
        ((declKey,declVal,bodyType) -> (declKey,declVal,bodyType)) next
    | RemoveDeclaration moduleKey declKey next
    
    | AddImport moduleKey bodyType ((importKey,importVal) -> next)
    | EditImport moduleKey importKey 
        ((importKey,importVal,bodyType) -> (importKey,importVal,bodyType)) next
    | RemoveImport moduleKey importKey next
    
    | AddExport moduleKey bodyType ((exportKey,exportVal) -> next)
    | EditExport moduleKey exportKey 
        ((exportKey,exportVal,bodyType) -> (exportKey,exportVal,bodyType)) next
    | RemoveExport moduleKey exportKey next
    | ExportAll moduleKey next
    
    | GetModules ([moduleKey] -> next)
    | GetDeclarations moduleKey ([declKey] -> next)
    | GetImports moduleKey ([importKey] -> next)
    | GetExports moduleKey (Maybe [exportKey] -> next)
    
    | GetDeclaration moduleKey declKey ((declKey,declVal,bodyType) -> next)
    | GetImport moduleKey importKey ((importKey,importVal,bodyType) -> next)
    | GetExport moduleKey exportKey ((exportKey,exportVal,bodyType) -> next)
    deriving (Functor)

{-
instance Functor (ProjectAST b) where
    fmap f (AddModule mi next) = AddModule mi (f next)
    fmap f (RemoveModule mi next) = RemoveModule mi (f next)
    fmap f (AddDeclaration mi di d next) = AddDeclaration mi di d (f next)
-}

  
newtype ProjectMonad 
        projectNew projectLoad projectSave
        moduleKey
        declKey declVal
        importKey importVal
        exportKey exportVal
        bodyType
        r
    = MkProjectMonad
        ( Free 
            ( ProjectAST
                projectNew projectLoad projectSave
                moduleKey
                declKey declVal
                importKey importVal
                exportKey exportVal
                bodyType
            ) r
        )
    deriving (Functor, Applicative, Monad)


{-
addModule :: ModuleInfo -> ProjectMonad () ()
addModule mi = MkProjectMonad $ liftF (AddModule mi ())
removeModule :: ModuleInfo -> ProjectMonad () ()
removeModule mi = MkProjectMonad $ liftF (RemoveModule mi ())
addDeclaration:: ModuleInfo -> DeclarationInfo -> Declaration -> ProjectMonad () ()
addDeclaration mi di d = MkProjectMonad $ liftF (AddDeclaration mi di d ())
-}

createProject pn = MkProjectMonad $ liftF $ CreateProject pn ()
loadProject pl = MkProjectMonad $ liftF $ LoadProject pl ()
saveProject ps = MkProjectMonad $ liftF $ SaveProject ps ()


createModule mk = MkProjectMonad $ liftF $ CreateModule mk () 
removeModule mk = MkProjectMonad $ liftF $ RemoveModule mk ()

{-
createExternModule mk = MkProjectMonad $ liftF $ CreateExternModule mk ()
removeExternModule mk = MkProjectMonad $ liftF $ RemoveExternModule mk ()
-}

createDeclaration mk b = MkProjectMonad $ liftF $ CreateDeclaration mk b id
editDeclaration mk dk f = MkProjectMonad $ liftF $ EditDeclaration mk dk f ()
removeDeclaration mk dk = MkProjectMonad $ liftF $ RemoveDeclaration mk dk ()

addImport mk b = MkProjectMonad $ liftF $ AddImport mk b id
editImport mk ik f = MkProjectMonad $ liftF $ EditImport mk id f ()
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

getAllDeclarations = do
    mks <- getModules
    declPartitions <- forM mks getDeclarations
    return $ concat declPartitions
dumpModule mk = do
    dks <- getDeclarations mk 
    iks <- getImports mk
    eks <- getExports mk
    ds <- forM dks $ liftM snd3 . getDeclaration mk
    is <- forM iks $ liftM snd3 . getImport mk
    es <- case eks of
        Nothing -> return Nothing
        Just eks -> liftM Just $ forM eks $ liftM snd3 . getExport mk
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
    db <- liftM snd3 $ getDeclaration mksrc dk
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
