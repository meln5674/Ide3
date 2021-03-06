{-|
Module      : Ide3.ModuleTree
Description : Tree structure for modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

The ModuleTree type represents a tree of modules with the period separated
symbols specifying the heirarchy.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide3.ModuleTree where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as T

import Data.Monoid

import Control.Monad

import Ide3.NewMonad
import Ide3.Types.Internal
import Ide3.SrcLoc.Types

-- | Utility function, just map with arguments reversed
for :: [a] -> (a -> b) -> [b]
for xs f = map f xs

-- | Given a list and a function which produces a key from a list item,
-- Create a map which maps a key to the list of inputs which all produced that
-- same key
partitionBy :: Ord k => (a -> k) -> [a] -> Map k [a]
partitionBy f = foldl addOrCreate Map.empty
  where
    addOrCreate m x = Map.alter newIfEmpty (f x) m
      where
        newIfEmpty Nothing = Just [x]
        newIfEmpty (Just ys) = Just (x:ys)
-- | A tree representing a heirarchy of modules, along with the keys for
-- reterieving their contents
data ModuleTree
    = OrgNode
        ModuleInfo 
        [ModuleTree]
    | ModuleNode
        ModuleInfo 
        [ModuleTree] 
        [Pragma] 
        [DeclarationInfo] 
        [(ImportId, WithBody Import)] 
        (Maybe [(ExportId,WithBody Export)])
    | UnparsableModuleNode
        ModuleInfo
        [ModuleTree]
        Text
        SrcLoc
        String
    deriving Show

-- | Take a list of module infos and produce a module tree with no declarations
makeTreeSkeleton :: [ModuleInfo] -> [ModuleTree]
makeTreeSkeleton = go ""
  where
    go knownRoot modInfos = map (uncurry processPartition) partitions
      where
        processPartition rootInfo subModuleNames = if rootPresent
            then ModuleNode rootInfo subTrees [] [] [] Nothing
            else OrgNode rootInfo subTrees
          where
            subTrees = go newRoot toProcess
            rootPresent = rootInfo `elem` subModuleNames
            ModuleInfo (Symbol rootString) = rootInfo
            newRoot = rootString <> "."
            toProcess = if rootPresent
                then delete rootInfo subModuleNames
                else subModuleNames
        -- A partition is a set of modules which all have the same root module,
        -- e.g. Data.Map, Data.Vector, and Data.Map.Lazy are in the same parition
        -- e.g. Data.Map and OtherData.Map are not
        partitions = Map.toList $ partitionBy getRootName modInfos
        getRootName (ModuleInfo (Symbol s)) = ModuleInfo 
                                            $ Symbol 
                                            $ preRoot <> postRoot
          where
            preRoot = T.take (T.length knownRoot) s
            postRoot = T.takeWhile ('.' /= ) $ T.drop (T.length knownRoot) s

-- | Take a module tree and fill each node with its declarations
fillTree :: ( ModuleExportClass m
            , ModuleImportClass m
            , ModuleDeclarationClass m
            , ModulePragmaClass m
            , ProjectModuleClass m
            )
         => ProjectInfo 
         -> ModuleTree 
         -> SolutionResult u m ModuleTree
fillTree pji (OrgNode i ts) = do
    ts' <- mapM (fillTree pji) ts
    return $ OrgNode i ts'
fillTree pji (ModuleNode i ts _ _ _ _) = do
    maybeContents <- getUnparsableModule pji i
    case maybeContents of
        Just (contents, loc, msg) -> do
            ts' <- mapM (fillTree pji) ts
            return $ UnparsableModuleNode i ts' contents loc msg
        Nothing -> do
            ps <- getPragmas pji i
            ds <- getDeclarations pji i
            iids <- getImports pji i
            eids <- getExports pji i
            is <- forM iids $ \iid -> (,) iid <$> getImport pji i iid
            es <- case eids of
                Nothing -> return Nothing
                Just eids' -> do
                    x <- forM eids' $ \eid -> (,) eid <$> getExport pji i eid
                    return $ Just x
            ts' <- mapM (fillTree pji) ts
            return $ ModuleNode i ts' ps ds is es
fillTree pji (UnparsableModuleNode i ts s loc msg) = do
    ts' <- mapM (fillTree pji) ts
    return $ UnparsableModuleNode i ts' s loc msg

-- | Generate a tree for specific module
makeModuleTree :: ( ProjectModuleClass m
            , ModuleExportClass m
            , ModuleImportClass m
            , ModuleDeclarationClass m
            , ModulePragmaClass m
            )
         => ProjectInfo
         -> ModuleInfo
         -> SolutionResult u m ModuleTree
makeModuleTree pji mi = do
    let [emptyTree] = makeTreeSkeleton [mi]
    fillTree pji emptyTree

-- | Make a module tree from a project
makeTree :: ( ProjectModuleClass m
            , ModuleExportClass m
            , ModuleImportClass m
            , ModuleDeclarationClass m
            , ModulePragmaClass m
            )
         => ProjectInfo 
         -> SolutionResult u m [ModuleTree]
makeTree pji = do
    modules <- getModules pji
    let emptyTree = makeTreeSkeleton modules
    mapM (fillTree pji) emptyTree

-- | Format a module tree as plain text
formatTree :: ModuleTree -> Text
formatTree = T.intercalate "\n" . go []
  where
    go prefixFlags tree = allLines
      where
        buildPrefix [] = ""
        buildPrefix (True:xs)  = buildPrefix xs <> "|   "
        buildPrefix (False:xs) = buildPrefix xs <> "    "
        prefix = buildPrefix prefixFlags
        headPrefix = buildPrefix (drop 1 prefixFlags)
        decls = case tree of 
            ModuleNode _ _ _ ds _ _ -> ds
            _ -> []
        subModules = case tree of
            OrgNode _ ms -> ms
            ModuleNode _ ms _ _ _ _ -> ms
            UnparsableModuleNode _ ms _ _ _ -> ms
        moduleInfo = case tree of
            OrgNode n _ -> n
            ModuleNode n _ _ _ _ _ -> n
            UnparsableModuleNode n _ _ _ _ -> n
        ModuleInfo (Symbol moduleName) = moduleInfo
        firstLine = case prefixFlags of
            [] -> moduleName
            [_] -> "+-- " <> moduleName
            _ -> headPrefix <> "+-- " <> moduleName
        declLines = case decls of
            [] -> []
            ds -> map ((prefix <>) . ("|- " <>) . makeLine) ds
              where
                makeLine (SymbolDeclarationInfo (Symbol s)) = s
                makeLine (RawDeclarationInfo s) = s
        subModuleLines = case subModules of
            [] -> []
            ms -> firstModuleLines ++ lastModuleLines
              where
                firstModules = init ms
                lastModule = last ms
                firstModuleLines = concatMap (go (True:prefixFlags)) 
                                             firstModules
                lastModuleLines = go (False:prefixFlags) lastModule
        allLines = case (declLines,subModuleLines) of
            ([],[]) -> [firstLine,prefix]
            (ds,[]) -> firstLine : [prefix <> "|"] ++ ds ++ [prefix]
            ([],ms) -> firstLine : [prefix <> "|"] ++ ms ++ [prefix]
            (ds,ms) -> firstLine 
                     : [prefix <> "|"] 
                     ++ ds 
                     ++ [prefix <> "|"] 
                     ++ ms 
                     ++ [prefix]

