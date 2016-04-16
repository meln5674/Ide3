{-# LANGUAGE ImplicitParams #-}
module Tests.Utils where

import GHC.Stack

import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Test.HUnit

import Ide3.Types
import Ide3.Monad
import Ide3.Mechanism
import Ide3.Mechanism.State

import qualified Ide3.Project as Project

instance Monad m => ProjectShellM (StateT s m) where
    load = error "CAN'T LOAD"
    new = return . Project.new
    finalize = error "CAN'T FINALIZE"

failedTestName :: (?loc :: CallStack) => String
failedTestName = fst . last . getCallStack $ ?loc

runExceptProject :: ProjectResult ProjectState () a -> ProjectState (Either (ProjectError ()) a)
runExceptProject = runExceptT

runExceptProjectT :: Monad m => ProjectResult (ProjectStateT m) () a -> ProjectStateT m (Either (ProjectError ()) a)
runExceptProjectT = runExceptT

expectFailure :: (?loc :: CallStack)
              => (Show a) 
              => ProjectResult (ProjectStateT IO) () a 
              -> Test
expectFailure f = TestCase $ do
    (result,_) <- runProjectStateT $ runExceptProjectT f
    case result of
        Left _ -> assertString ""
        Right x -> assertFailure $ failedTestName ++ ": Expected to fail but got: " ++ show x

expectSuccess :: (?loc :: CallStack)
              => (Show a)
              => ProjectResult (ProjectStateT IO) () a -> Test
expectSuccess f = TestCase $ do
    (result,_) <- runProjectStateT $ runExceptProjectT f
    case result of
        Left err -> assertFailure $ failedTestName ++ ": Expected to succeed but got: " ++ show err
        Right _ -> assertString ""

expectResult :: (?loc :: CallStack)
             => (Show a, Eq a) 
             => a -> ProjectResult (ProjectStateT IO) () a -> Test
expectResult expected f = TestCase $ do
    (result,_) <- runProjectStateT $ runExceptProjectT f
    case result of
        Left err -> assertFailure $ failedTestName ++ ": Expected to succeed but got: " ++ show err
        Right actual | expected == actual -> assertString ""
                     | otherwise -> assertFailure 
                                 $ "Succeeded, but got " 
                                 ++ show actual 
                                 ++ " when expecting " 
                                 ++ show expected

unimplementedTest :: (?loc :: CallStack)
                  => Test
unimplementedTest = TestCase $ assertFailure $ "Test not yet Implemented: " ++ failedTestName

nonExistentModuleInfo = ModuleInfo $ Symbol "Non.Existent.Module"
newModuleInfo = ModuleInfo $ Symbol "New.Module"
newDeclarationInfo = DeclarationInfo $ Symbol "TestType"
newDeclaration = WithBody
    (TypeDeclaration newDeclarationInfo
                    $ TypeSynonym (Symbol "TestType")
                                  (Symbol "String"))
    "type TestType = String"
nonExistentDeclarationInfo = DeclarationInfo $ Symbol "NonExistentType"
nonExistentDeclaration = WithBody
    (TypeDeclaration nonExistentDeclarationInfo
                    $ TypeSynonym (Symbol "NonExistentType")
                                  (Symbol "String"))
    "type TestType = String" 
newExport = WithBody (SingleExport (Symbol "testSymbol")) "testSymbol"
