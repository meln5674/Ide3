{-# LANGUAGE LambdaCase #-}
module Ide3.ModuleTree where

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad

import Ide3.Monad

import Ide3.Types hiding (moduleInfo)

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
    | ModuleNode ModuleInfo [ModuleTree] [Pragma] [DeclarationInfo] [(ImportId, WithBody Import)] (Maybe [(ExportId,WithBody Export)])
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
                UnamedModule _ -> error "Cannot make a tree with a pathless unamed module"
            newRoot = rootString ++ "."
            toProcess = if rootPresent
                then delete rootInfo subModuleNames
                else subModuleNames
            makeNode x y z = if rootPresent
                then ModuleNode x (go y z) [] [] [] Nothing
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
fillTree :: SolutionM m => ProjectInfo -> ModuleTree -> SolutionResult m u ModuleTree
fillTree pi (OrgNode i ts) = do
    ts' <- mapM (fillTree pi) ts
    return $ OrgNode i ts'
fillTree pi (ModuleNode i ts _ _ _ _) = do
    ps <- getPragmas pi i
    ds <- getDeclarations pi i
    iids <- getImports pi i
    eids <- getExports pi i
    is <- forM iids $ \iid -> liftM ((,) iid) $ getImport pi i iid
    es <- case eids of
        Nothing -> return Nothing
        Just eids' -> do
            x <- forM eids' $ \eid -> liftM ((,) eid) $ getExport pi i eid
            return $ Just x
    ts' <- mapM (fillTree pi) ts
    return $ ModuleNode i ts' ps ds is es

-- | Make a module tree from the current project
makeTree :: SolutionM m => ProjectInfo -> SolutionResult m u [ModuleTree]
makeTree pi = do
    modules <- getModules pi
    let emptyTree = makeTreeSkeleton modules
    mapM (fillTree pi) emptyTree

-- | Format a module tree as plain text
formatTree :: ModuleTree -> String
formatTree = intercalate "\n" . go []
  where
    go prefixFlags tree = allLines
      where
        buildPrefix [] = ""
        buildPrefix (True:xs)  = buildPrefix xs ++ "|   "
        buildPrefix (False:xs) = buildPrefix xs ++ "    "
        prefix = buildPrefix prefixFlags
        headPrefix = buildPrefix (drop 1 prefixFlags)
        decls = case tree of 
            OrgNode{} -> []
            ModuleNode _ _ _ ds _ _ -> ds
        subModules = case tree of
            OrgNode _ ms -> ms
            ModuleNode _ ms _ _ _ _ -> ms
        moduleInfo = case tree of
            OrgNode n _ -> n
            ModuleNode n _ _ _ _ _ -> n
        moduleName = case moduleInfo of
            ModuleInfo (Symbol n) -> n
            UnamedModule (Just p) -> p
            UnamedModule _ -> error "Cannot make a tree with a pathless unamed module"
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
        allLines = case (declLines,subModuleLines) of
            ([],[]) -> [firstLine,prefix]
            (ds,[]) -> firstLine : [prefix ++ "|"] ++ ds ++ [prefix]
            ([],ms) -> firstLine : [prefix ++ "|"] ++ ms ++ [prefix]
            (ds,ms) -> firstLine : [prefix ++ "|"] ++ ds ++ [prefix ++ "|"] ++ ms ++ [prefix]

