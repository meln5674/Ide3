{-# LANGUAGE LambdaCase #-}
module ModuleTree where

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Ide3.Monad

import Ide3.Types

-- | Utility function, just map with arguments reversed
for :: [a] -> (a -> b) -> [b]
for xs f = map f xs

-- | Given a list and a function which produces a key from a list item,
-- Create a map which maps a key to the list of inputs which all produced that same key
partitionBy :: Ord k => (a -> k) -> [a] -> Map k [a]
partitionBy f = foldl (\m x -> Map.alter (\case { Nothing -> Just [x]; Just ys -> Just (x:ys) }) (f x) m) Map.empty

-- | Data type used by the tree command
data ModuleTree
    = OrgNode ModuleInfo [ModuleTree]
    | ModuleNode ModuleInfo [DeclarationInfo] [ModuleTree]
    deriving Show

-- | Take a list of module infos and produce a module tree with no declarations
makeTreeSkeleton :: [ModuleInfo] -> [ModuleTree]
makeTreeSkeleton = go ""
  where
    go knownRoot modInfos = for partitions processPartition
      where
        processPartition (rootInfo,subModuleNames) = makeNode rootInfo newRoot toProcess
          where
            rootPresent = rootInfo `elem` subModuleNames
            rootString = case rootInfo of
                ModuleInfo (Symbol s) -> s
                UnamedModule (Just p) -> p
            newRoot = rootString ++ "."
            toProcess = if rootPresent
                then delete rootInfo subModuleNames
                else subModuleNames
            makeNode x y z = if rootPresent
                then ModuleNode x [] $ go y z
                else OrgNode x $ go y z
        partitions = Map.toList $ partitionBy getRootName modInfos
        getRootName (ModuleInfo (Symbol s)) = ModuleInfo $ Symbol $ preRoot ++ postRoot
          where
            preRoot = take (length knownRoot) s
            postRoot = takeWhile ('.' /= ) $ drop (length knownRoot) s
        getRootName x = x


{-
Take a list of module names
Find all root module names
Collect module names into lists tagged with the root
For each list:
    If the root is present:
        remove the root
        Repeat the process on the sub-list, not considering the root as part of the name
        Collect the result into a ModuleNode with empty declarations
    If the root is not present:
        Repeat the process on the sub-list, not considering the root as part of the name
        Collect the result into an OrgNode

-}

-- | Take a module tree and fill each node with its declarations
fillTree :: ProjectM m => ModuleTree -> ProjectResult m u ModuleTree
fillTree (OrgNode i ts) = do
    ts' <- mapM fillTree ts
    return $ OrgNode i ts'
fillTree (ModuleNode i _ ts) = do
    ds <- getDeclarations i
    ts' <- mapM fillTree ts
    return $ ModuleNode i ds ts'

-- | Make a module tree from the current project
makeTree :: ProjectM m => ProjectResult m u [ModuleTree]
makeTree = do
    modules <- getModules
    let emptyTree = makeTreeSkeleton modules
    mapM fillTree emptyTree

-- | Format a module tree as plain text
formatTree :: ModuleTree -> String
formatTree = intercalate "\n" . go []
  where
    go prefixFlags tree = lines
      where
        buildPrefix [] = ""
        buildPrefix (True:xs)  = buildPrefix xs ++ "|   "
        buildPrefix (False:xs) = buildPrefix xs ++ "    "
        prefix = buildPrefix prefixFlags
        headPrefix = buildPrefix (drop 1 prefixFlags)
        decls = case tree of 
            OrgNode{} -> []
            ModuleNode _ ds _ -> ds
        subModules = case tree of
            OrgNode _ ms -> ms
            ModuleNode _ _ ms -> ms
        moduleInfo = case tree of
            OrgNode n _ -> n
            ModuleNode n _ _ -> n
        moduleName = case moduleInfo of
            ModuleInfo (Symbol n) -> n
            UnamedModule (Just p) -> p
        firstLine = case prefixFlags of
            [] -> moduleName
            [_] -> "+-- " ++ moduleName
            _ -> headPrefix ++ "+-- " ++ moduleName
        declLines = case decls of
            [] -> []
            ds -> map ((prefix ++) . ("|- " ++) . makeLine) ds
              where
                makeLine (DeclarationInfo (Symbol s)) = s
        subModuleLines = case subModules of
            [] -> []
            ms -> firstModuleLines ++ lastModuleLines
              where
                firstModules = init ms
                lastModule = last ms
                firstModuleLines = concatMap (go (True:prefixFlags)) firstModules
                lastModuleLines = go (False:prefixFlags) lastModule
        lines = case (declLines,subModuleLines) of
            ([],[]) -> [firstLine,prefix]
            (ds,[]) -> firstLine : [prefix ++ "|"] ++ ds ++ [prefix]
            ([],ms) -> firstLine : [prefix ++ "|"] ++ ms ++ [prefix]
            (ds,ms) -> firstLine : [prefix ++ "|"] ++ ds ++ [prefix ++ "|"] ++ ms ++ [prefix]

