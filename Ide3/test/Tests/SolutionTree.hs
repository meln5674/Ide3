{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Tests.SolutionTree where

import Data.Tree

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict

import Test.HUnit

import Ide3.Types

import SolutionTree
import DeclarationPath

import GuiClass

data ForestZipper a 
    = ForestZipper 
    { forestPrevious :: Forest a
    , forestNext :: Forest a
    , zipperRootLabel :: a
    , zipperSubForest :: Forest a
    }
    | ForestTreeZipper
    { forestPrevious :: Forest a
    , forestNext :: Forest a
    , zipperRootLabel :: a
    , forestZipper :: ForestZipper a
    }

unzipForest :: TreePath -> Forest a -> Maybe (ForestZipper a)
unzipForest [] ts = Nothing
unzipForest [ix] ts
    | 0 <= ix && ix < length ts 
        = Just $ ForestZipper (take ix ts) 
                              (drop (ix+1) ts) 
                              (rootLabel (ts !! ix))
                              (subForest (ts !! ix))
    | otherwise = Nothing
unzipForest (ix:ixs) ts
    | 0 <= ix && ix < length ts 
        = liftM (ForestTreeZipper (take ix ts) 
                                  (drop (ix+1) ts) 
                                  (rootLabel (ts !! ix))
                ) 
              $ unzipForest ixs (subForest $ ts !! ix)
    | otherwise = Nothing

rezipForest :: Functor f => (a -> Forest a -> f (Tree a)) -> ForestZipper a -> f (Forest a)
rezipForest f (ForestZipper prev next lbl ts) 
    =   (prev ++) 
    .   (: next) 
    <$> f lbl ts
rezipForest f (ForestTreeZipper prev next lbl zip) 
    =   (prev ++) 
    .   (: next) 
    .   Node lbl 
    <$> rezipForest f zip
    
data MockSolutionViewState = MockSolutionViewState { mockForest :: Forest SolutionTreeElem }

newtype MockSolutionViewT m a = MockSolutionViewT
    { runMockSolutionViewT :: StateT MockSolutionViewState (MaybeT m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans MockSolutionViewT where
    lift = MockSolutionViewT . lift . lift

traverseForest :: TreePath -> Forest a -> Maybe (Forest a)
traverseForest [] ts = Just ts
traverseForest path ts = liftM subForest $ traverseForestToTree path ts


traverseForestToNode :: TreePath -> Forest a -> Maybe a
traverseForestToNode path ts = liftM rootLabel $ traverseForestToTree path ts
    

traverseForestToTree:: TreePath -> Forest a -> Maybe (Tree a)
traverseForestToTree [] _ = Nothing
traverseForestToTree [ix] ts
    | 0 <= ix && ix < length ts = Just $ ts !! ix
    | otherwise = Nothing
traverseForestToTree (ix:ixs) ts
    | 0 <= ix && ix < length ts = traverseForestToTree ixs $ subForest $ ts !! ix
    | otherwise = Nothing

instance Monad m => SolutionViewClass (MockSolutionViewT m) where
    getElemAtSolutionTreePath path 
        = MockSolutionViewT 
        $ gets mockForest >>= (lift . MaybeT . return . traverseForestToNode path)
    getTreeAtSolutionTreePath path 
        = MockSolutionViewT 
        $ gets mockForest >>= (lift . MaybeT . return . traverseForestToTree path)
    getForestAtSolutionTreePath path 
        = MockSolutionViewT 
        $ gets mockForest >>= (lift . MaybeT . return . traverseForest path)
    lookupAtSolutionTreePath path 
        = MockSolutionViewT 
        $ gets mockForest >>= (lift . lift . return . traverseForestToTree path)
    setSolutionTree
        = MockSolutionViewT 
        . put
        . MockSolutionViewState
    updateSolutionTreePathNode path f
        = MockSolutionViewT $ do
            zipper <- gets mockForest >>= lift . MaybeT . return . unzipForest path
            ts' <- lift $ MaybeT $ return $ flip rezipForest zipper $ \lbl ts -> Just $ Node (f lbl) ts
            put $ MockSolutionViewState $ ts'
    insertSolutionTreePathNode [] ix item = MockSolutionViewT $ do
        ts' <- gets mockForest >>= lift . MaybeT . return . \ts -> case ix of
            Just ix
                | 0 <= ix && ix < length ts 
                    -> Just $ (take ix ts) ++ (Node item []) : (drop ix ts)
                | otherwise -> Nothing
            Nothing -> Just $ ts ++ [Node item []]
        put $ MockSolutionViewState ts'
    insertSolutionTreePathNode path ix item
        = MockSolutionViewT $ do
            zipper <- gets mockForest >>= lift . MaybeT . return . unzipForest path
            ts' <- lift $ MaybeT $ return $ flip rezipForest zipper $ \lbl ts -> case ix of
                Just ix | 0 <= ix && ix < length ts 
                    -> Just $ Node lbl 
                                 $  (take ix ts) 
                                 ++ (Node item [])
                                 :  (drop ix ts)
                        | otherwise -> Nothing
                Nothing -> Just $ Node lbl $ ts ++ [Node item []]
            put $ MockSolutionViewState ts'
    insertSolutionTreePathTree path ix t
        = MockSolutionViewT $ do
            zipper <- gets mockForest >>= lift . MaybeT . return . unzipForest path
            ts' <- lift $ MaybeT $ return $ flip rezipForest zipper $ \lbl ts -> case ix of
                Just ix | 0 <= ix && ix < length ts 
                    -> Just $ Node lbl 
                                 $  (take ix ts) 
                                 ++ t
                                 :  (drop ix ts)
                        | otherwise -> Nothing
                Nothing -> Just $ Node lbl $ ts ++ [t]
            put $ MockSolutionViewState ts'
    removeSolutionTreePathNode [] = MockSolutionViewT $ lift $ MaybeT $ return Nothing
    removeSolutionTreePathNode path' = MockSolutionViewT $ do
        let path = init path'
            ix = last path'
        zipper <- gets mockForest >>= lift . MaybeT . return . unzipForest path
        ts' <- lift $ MaybeT $ return $ flip rezipForest zipper $ \lbl ts -> case () of
            _ | 0 <= ix && ix < length ts
                -> Just $ Node lbl $ (take ix ts) ++ (drop (ix+1) ts)
              | otherwise -> Nothing
        put $ MockSolutionViewState ts'




runSolutionTreeTest :: Maybe String
				 	-> (Maybe (a, Forest SolutionTreeElem) -> String) 
				 	-> (forall m . SolutionViewClass m => m a) 
				 	-> Test
runSolutionTreeTest lbl getMessage action = testContainer $ do
	result' <- runMaybeT $ runStateT (runMockSolutionViewT action) (MockSolutionViewState [])
	let result = fmap (fmap mockForest) result'
	assertString $ getMessage result
  where
    testContainer = case lbl of
        Just lbl -> TestLabel lbl . TestCase
        Nothing -> TestCase

runSolutionTreeTest' :: Maybe String
                     -> Forest SolutionTreeElem
                     -> (forall m . SolutionViewClass m => m a) 
                     -> Test
runSolutionTreeTest' lbl expected = runSolutionTreeTest lbl $ \case
    Just (_,actual)
        | actual == expected -> ""
        | otherwise -> "\tExpected: " ++ show expected ++ "\n\tActual: " ++ show actual
    Nothing -> "Operation Failed"

test_insertProject :: Test
test_insertProject = runSolutionTreeTest' (Just "Insert Project") expected $ do
    insertSolutionTreeNode SolutionPath testProjectElem
  where
    testProjectInfo = ProjectInfo "test"
    testProjectElem = ProjectElem testProjectInfo
    expected = [Node testProjectElem []]
