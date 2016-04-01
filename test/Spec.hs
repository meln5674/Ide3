import Control.Monad.State

import Control.Monad.Trans.Except

import Ide3.Mechanism
import Ide3.Types
import Ide3.Monad
import qualified Ide3.Module as Module
import qualified Ide3.Project as Project

testModuleName = "Test"
testModuleName2 = "Test2"
testTypeSynonym = "type Test = String"
testTypeSynonym2 = "type Test2 = Int"
testNewtype = "newtype TestNewtype = MkTestNewtype Test"
testModuleInfo = ModuleInfo (Symbol testModuleName)
testModuleInfo2 = ModuleInfo (Symbol testModuleName2)
testImport = "import Test2"
testExport = "Test"
testExport2 = "Test2"

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

main :: IO ()
main = do
    --runTest test2 [testModuleInfo2]
    runTest test1 [testModuleInfo, testModuleInfo2]
    --print testPrim
