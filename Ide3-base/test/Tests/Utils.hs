{-# LANGUAGE ImplicitParams #-}
module Tests.Utils where

import GHC.Stack

import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Test.HUnit

import Ide3.Types
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State
import Ide3.NewMonad.Instances.State.Class
import Ide3.NewMonad.Instances.State.Class.Instances.Strict

import qualified Ide3.Solution as Solution
import qualified Ide3.Env.Solution as Solution

instance Monad m => StatefulPersistenceClass (SolutionStateT m) where
    loadState = error "CAN'T LOAD"
    newState = return . Solution.new
    finalizeState = error "CAN'T FINALIZE"

failedTestName :: (?loc :: CallStack) => String
failedTestName = fst . last . getCallStack $ ?loc

{-
runExceptProject :: SolutionResult (StatefulWrapper SolutionState) () a 
                 -> SolutionState (Either (SolutionError ()) a)
runExceptProject = runStatefulWrapper . runExceptT
-}

runExceptProjectT :: Monad m => SolutionResult (StatefulWrapper (SolutionStateT m)) () a 
                  -> SolutionStateT m (Either (SolutionError ()) a)
runExceptProjectT = runStatefulWrapper . runExceptT

expectFailure :: (?loc :: CallStack, Show a)
              => SolutionResult (StatefulWrapper (SolutionStateT IO)) () a 
              -> Test
expectFailure f = TestCase $ do
    (result,_) <- flip runStateT Solution.empty $ runSolutionStateT $ runExceptProjectT f
    case result of
        Left _ -> assertString ""
        Right x -> assertFailure $ failedTestName ++ ": Expected to fail but got: " ++ show x

expectSuccess :: (?loc :: CallStack, Show a)
              => SolutionResult (StatefulWrapper (SolutionStateT IO)) () a 
              -> Test
expectSuccess f = TestCase $ do
    (result,_) <- flip runStateT Solution.empty $ runSolutionStateT  $ runExceptProjectT f
    case result of
        Left err -> assertFailure $ failedTestName ++ ": Expected to succeed but got: " ++ show err
        Right _ -> assertString ""

expectResult :: (?loc :: CallStack, Show a, Eq a)
             => a -> SolutionResult (StatefulWrapper (SolutionStateT IO)) () a 
             -> Test
expectResult expected f = TestCase $ do
    (result,_) <- flip runStateT Solution.empty $ runSolutionStateT  $ runExceptProjectT f
    case result of
        Left err -> assertFailure $ failedTestName ++ ": Expected to succeed but got: " ++ show err
        Right actual | expected == actual -> assertString ""
                     | otherwise -> assertFailure 
                                 $  failedTestName 
                                 ++ ": Succeeded, but got " 
                                 ++ show actual 
                                 ++ " when expecting " 
                                 ++ show expected

expectPredicate :: (?loc :: CallStack, Show a, Eq a)
                => (a -> Bool) -> SolutionResult (StatefulWrapper (SolutionStateT IO)) () a 
                -> Test
expectPredicate predicate f = TestCase $ do
    (result,_) <- flip runStateT Solution.empty $ runSolutionStateT  $ runExceptProjectT f
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

nonExistentProjectInfo = ProjectInfo "non-existent-project"
newProjectInfo = ProjectInfo "untitled project"
newProjectInfo2 = ProjectInfo "untitled project 2"

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
