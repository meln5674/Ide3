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
                                 $  failedTestName 
                                 ++ ": Succeeded, but got " 
                                 ++ show actual 
                                 ++ " when expecting " 
                                 ++ show expected

expectPredicate :: (?loc :: CallStack)
                => (Show a, Eq a)
                => (a -> Bool) -> ProjectResult (ProjectStateT IO) () a -> Test
expectPredicate predicate f = TestCase $ do
    (result,_) <- runProjectStateT $ runExceptProjectT f
    case result of
        Left err -> assertFailure $ failedTestName ++ ": Expected to succeed but got : " ++ show err
        Right actual | predicate actual -> assertString ""
                     | otherwise -> assertFailure
                                 $  failedTestName
                                 ++ ": Succeeded, but result "
                                 ++ show actual
                                 ++ " did not pass the predicate"

unimplementedTest :: (?loc :: CallStack)
                  => Test
unimplementedTest = TestCase $ assertFailure $ "Test not yet Implemented: " ++ failedTestName

nonExistentModuleInfo = ModuleInfo $ Symbol "Non.Existent.Module"
newModuleInfo = ModuleInfo $ Symbol "New.Module"
newModuleInfo2 = ModuleInfo $ Symbol "New.Module2"
newDeclarationInfo = DeclarationInfo newDeclarationSymbol
newDeclarationSymbol = Symbol "TestType"
newDeclaration = WithBody
    (TypeDeclaration newDeclarationInfo
                    $ TypeSynonym (Symbol "TestType")
                                  (Symbol "String"))
    "type TestType = String"
newDeclarationExport = WithBody (SingleExport newDeclarationSymbol) "TestType"
newDeclarationImport = WithBody (WhitelistImport (Symbol "New.Module2") False Nothing [NameImport newDeclarationSymbol]) "import New.Module2 (TestType)"


nonExistentDeclarationInfo = DeclarationInfo $ Symbol "NonExistentType"
nonExistentDeclaration = WithBody
    (TypeDeclaration nonExistentDeclarationInfo
                    $ TypeSynonym (Symbol "NonExistentType")
                                  (Symbol "String"))
    "type TestType = String" 
newExport = WithBody (SingleExport (Symbol "testSymbol")) "testSymbol"
nonExistentExportId = -1 :: Int

newImport = WithBody (ModuleImport (Symbol "Test.Module") False Nothing) "import Test.Module"
nonExistentImportId = -1 :: Int

newModule2Import = WithBody (ModuleImport (Symbol "New.Module2") False Nothing) "import New.Module2"
nonExistentDeclarationExport = WithBody (SingleExport (Symbol "NonExistentType")) "NonExistentType"


newCompoundDeclarationSymbols = map Symbol ["TestData", "TestCon1", "TestCon2"]
newCompoundDeclarationSymbol = head newCompoundDeclarationSymbols
newCompoundDeclarationInfo = DeclarationInfo newCompoundDeclarationSymbol
newCompoundDeclarationConstructors = map (\s -> PrefixConstructor s []) $ tail newCompoundDeclarationSymbols
newCompoundDeclaration = WithBody (TypeDeclaration newCompoundDeclarationInfo $ DataDeclaration newCompoundDeclarationSymbol newCompoundDeclarationConstructors) "data TestData = TestCon1 | TestCon2"

newCompoundDeclarationIncluded = take 1 $ drop 1 $ newCompoundDeclarationSymbols
newCompoundDeclarationExcluded = take 1 $ drop 2 $ newCompoundDeclarationSymbols

newCompoundDeclarationPartialImport
    = WithBody 
        (WhitelistImport
            (Symbol "New.Module2") 
            False 
            Nothing 
            [AggregateImport 
                newCompoundDeclarationSymbol 
              $ Just newCompoundDeclarationIncluded
            ]
        )
        "import New.Module2 ( TestData (TestCon1) )"
newCompoundDeclarationPartialExport
    = WithBody 
        (AggregateExport
                newCompoundDeclarationSymbol 
              $ Just $ newCompoundDeclarationIncluded
        )
        "TestData (TestCon1)"
