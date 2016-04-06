import Test.HUnit

import Control.Monad.State

import Control.Monad.Trans.Except

import Ide3.Mechanism
import Ide3.Types
import Ide3.Monad
import qualified Ide3.Import as Import
import qualified Ide3.Export as Export
import qualified Ide3.Module as Module
import qualified Ide3.Project as Project

import qualified Data.Map as Map

testModuleName = "Test"
testModuleName2 = "Test2"
testTypeSynonym = "type Test = String"
testTypeSynonym2 = "type Test2 = Int"
testSymbol = Symbol "Test"
testSymbol2 = Symbol "Test2"
testNewtype = "newtype TestNewtype = MkTestNewtype Test"
testModuleInfo = ModuleInfo (Symbol testModuleName)
testModuleInfo2 = ModuleInfo (Symbol testModuleName2)
testImport = "import Test2"
testImportedModule = Symbol testModuleName2
testExport = "Test"
testExport2 = "Test2"
{-
type ProjectResult' a = [Either ProjectError a]
type ProjectResult = ( ProjectResult' String
                     , ProjectResult' [Symbol]
                     , ProjectResult' [Symbol]
                     , ProjectResult' [Declaration]
                     )


runTest' :: ProjectState () -> [ModuleInfo] -> ProjectResult
runTest' run infos = (\f -> f undefined) $ evalState $ do
    run
    files <- forM infos $ \info -> runExceptT $ Module.toFile <$> (ExceptT $ getModule info)
    esymss <- forM infos $ \info -> getExternalSymbols info
    isymss <- forM infos $ \info -> getInternalSymbols info
    declss <- forM infos $ \info -> runExceptT $ do
        m <- ExceptT $ getModule info
        let ds'' = map getChild $ Module.allDeclarations m
            ds' = mapM (Module.getDeclaration m) $ ds''
            ds2 = map (item . getChild) <$> ds'
        ExceptT $ return ds2
    return (files, esymss, isymss, declss)

runTest test infos = do
    let (files, esymss, isymss, declss) = runTest' test infos
    forM_ files $ \file -> case file of
            Right file -> do
                putStrLn "Module: "
                putStr (file++"\n")
                putStrLn ""
            Left msg -> putStrLn $ "TO FILE FAILED: " ++ msg
    forM_ esymss $ \esyms -> case esyms of
            Right esyms -> do
                putStrLn "External Symbols: "
                print esyms
                putStrLn ""
            Left msg -> putStrLn $ "EXTERNAL SYMBOLS FAILED: " ++ msg
    forM_ isymss $ \isyms -> case isyms of
            Right isyms -> do
                putStrLn "Internal Symbols: "
                print isyms
                putStrLn ""
            Left msg -> putStrLn $ "INTERNAL SYMBOLS FAILED: " ++ msg
    forM_ declss $ \decls -> case decls of
            Right decls -> do
                putStrLn "Declarations: "
                forM decls print
                putStrLn ""
            Left msg -> putStrLn $ "DECLARATIONS FAILED: " ++ msg

test1 :: ProjectState ()
test1 = do
    new ProjectInfo
    editProjectInfo id 
    createModule
            testModuleInfo
    createModule
            testModuleInfo2
    addRawDeclaration
            testModuleInfo
            testTypeSynonym
    addRawDeclaration
            testModuleInfo2
            testTypeSynonym2
    addRawImport
            testModuleInfo
            testImport
    addRawExport
            testModuleInfo2
            testExport2
    addRawDeclaration
            testModuleInfo2
            testNewtype


test2 :: ProjectState ()
test2 = do
    new ProjectInfo
    editProjectInfo id 
    createModule
        testModuleInfo2
    addRawDeclaration
        testModuleInfo2
        testTypeSynonym2
    addRawDeclaration
        testModuleInfo2
        testNewtype

testPrim = map (Module.getDeclaration mod . getChild) $ Module.allDeclarations mod 
  where
    decl1 = (WithBody (TypeDeclaration (DeclarationInfo (Symbol "A")) (TypeSynonym (Symbol "A") (Symbol "B"))) "")
    decl2 = (WithBody (TypeDeclaration (DeclarationInfo (Symbol "C")) (TypeSynonym (Symbol "C") (Symbol "D"))) "")
    mod = Module.addDeclaration (Module.addDeclaration Module.empty decl1) decl2
-}

testBase :: (Eq a, Show a) => ProjectState a -> a -> Test
testBase f expected = result ~=? expected
  where
    (result,_) = runProjectState f

testJust :: (Eq a, Show a) => ProjectState (Maybe a) -> a -> Test
testJust f expected = testBase f (Just expected)

testNothing :: (Eq a, Show a) => ProjectState (Maybe a) -> Test
testNothing f = testBase f Nothing

testRight :: (Eq r, Show r, Show l) => ProjectState (Either l r) -> r -> Test
testRight f expected = case result of
    Right r' -> r' ~=? expected
    Left l -> TestCase $ assertFailure (show l)
  where
    (result,_) = runProjectState f

testLeft :: (Eq r, Show r, Eq l, Show l) => ProjectState (Either l r) -> Test
testLeft f = case result of
    Right r -> TestCase $ assertFailure (show r)
    Left l -> TestCase $ assertBool "" True
  where
    (result,_) = runProjectState f
  

test_addModule :: Test
test_addModule = testRight f ()
  where
    f = do
        createModule testModuleInfo

test_addAndRetrieveModule :: Test
test_addAndRetrieveModule = testRight f expected
  where
    f = do
        createModule testModuleInfo
        getModule testModuleInfo
    expected = Module testModuleInfo (Map.empty) Nothing (Map.empty)

test_getNonexistentModule :: Test
test_getNonexistentModule = testLeft f
  where
    f = do
        getModule testModuleInfo

test_addAndRemoveModule :: Test
test_addAndRemoveModule = testRight f ()
  where
    f = do
        createModule testModuleInfo
        removeModule testModuleInfo

test_addRemoveThenGetModule :: Test
test_addRemoveThenGetModule = testLeft f
  where
    f = do
        createModule testModuleInfo
        removeModule testModuleInfo
        getModule testModuleInfo

test_addDuplicateModule :: Test
test_addDuplicateModule = testLeft f
  where
    f = do
        createModule testModuleInfo
        createModule testModuleInfo

test_removeNonexistentModule :: Test
test_removeNonexistentModule = testLeft f
  where
    f = do
        removeModule testModuleInfo

moduleTests =
    [ test_addModule
    , test_addAndRetrieveModule
    , test_getNonexistentModule
    , test_addAndRemoveModule
    , test_addRemoveThenGetModule
    , test_addDuplicateModule
    , test_removeNonexistentModule
    ]

test_addImport :: Test
test_addImport = testRight f True
  where
    f = runExceptT $ do
        ExceptT $ createModule testModuleInfo
        ExceptT $ addRawImport testModuleInfo testImport
        m <- (ExceptT $ getModule testModuleInfo)
        return $ m `Module.importsModule` testImportedModule

test_removeImport :: Test
test_removeImport = testRight f False
  where
    f = runExceptT $ do
        ExceptT $ createModule testModuleInfo
        id <- ExceptT $ addRawImport testModuleInfo testImport
        ExceptT $ removeImport testModuleInfo id
        m <- (ExceptT $ getModule testModuleInfo)
        return $ m `Module.importsModule` testImportedModule


test_importSymbolVisible :: Test
test_importSymbolVisible = testRight f True
    where
        f = runExceptT $ do
            ExceptT $ createModule testModuleInfo
            ExceptT $ createModule testModuleInfo2
            ExceptT $ addRawDeclaration testModuleInfo2 testTypeSynonym
            ExceptT $ addRawImport testModuleInfo testImport
            m <- ExceptT $ getModule testModuleInfo 
            syms <- Module.internalSymbols m 
            return $ testSymbol `elem` syms

testParse :: (Show e, Eq e, Show r, Eq r) => (String -> Either e r) -> String -> r -> Test
testParse p s r = p s ~?= Right r 

testParseImport = testParse Import.parse

test_importParse1 = testParseImport 
    "import X" 
    $ ModuleImport (Symbol "X") False Nothing
test_importParse2 = testParseImport 
    "import qualified X" 
    $ ModuleImport (Symbol "X") True Nothing
test_importParse3 = testParseImport 
    "import X as Y" 
    $ ModuleImport (Symbol "X") False (Just (Symbol "Y"))
test_importParse4 = testParseImport 
    "import qualified X as Y" 
    $ ModuleImport (Symbol "X") True (Just (Symbol "Y"))

test_importParse5 = testParseImport 
    "import X (Z)" 
    $ WhitelistImport (Symbol "X") False Nothing [NameImport (Symbol "Z")]
test_importParse6 = testParseImport 
    "import qualified X (Z)" 
    $ WhitelistImport (Symbol "X") True Nothing [NameImport (Symbol "Z")]
test_importParse7 = testParseImport 
    "import X as Y (Z)" 
    $ WhitelistImport (Symbol "X") False (Just (Symbol "Y")) [NameImport (Symbol "Z")]
test_importParse8 = testParseImport 
    "import qualified X as Y (Z)" 
    $ WhitelistImport (Symbol "X") True (Just (Symbol "Y")) [NameImport (Symbol "Z")]

test_importParse9 = testParseImport 
    "import X hiding (Z)" 
    $ BlacklistImport (Symbol "X") False Nothing [NameImport (Symbol "Z")]
test_importParse10 = testParseImport 
    "import qualified X hiding (Z)" 
    $ BlacklistImport (Symbol "X") True Nothing [NameImport (Symbol "Z")]
test_importParse11 = testParseImport 
    "import X as Y hiding (Z)" 
    $ BlacklistImport (Symbol "X") False (Just (Symbol "Y")) [NameImport (Symbol "Z")]
test_importParse12 = testParseImport 
    "import qualified X as Y hiding (Z)" 
    $ BlacklistImport (Symbol "X") True (Just (Symbol "Y")) [NameImport (Symbol "Z")]

importTests =
    [ test_addImport
    , test_removeImport
    , test_importSymbolVisible
    , test_importParse1
    , test_importParse2
    , test_importParse3
    , test_importParse4
    , test_importParse5
    , test_importParse6
    , test_importParse7
    , test_importParse8
    , test_importParse9
    , test_importParse10
    , test_importParse11
    , test_importParse12
    ]

allTests = concat
    [ moduleTests
    , importTests
    ]




main :: IO ()
main = void $ runTestTT $ TestList allTests
