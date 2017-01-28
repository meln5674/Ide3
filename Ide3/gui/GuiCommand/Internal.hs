{-|
Module      : GuiCommand.Internal
Description : High-level operations peformed in response to user actions
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module GuiCommand.Internal where

import Data.Monoid
import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as T

import qualified Ide3.OrderedMap as OMap

import Data.Char

import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types
import Ide3.Utils
import Ide3.NewMonad

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Solution as Solution (findSymbol)
import qualified Ide3.Import as Import
import qualified Ide3.Export as Export

import qualified Ide3.ModuleTree as M (makeModuleTree)

import Builder
import Initializer
import Runner

import EnvironmentMonad

import Viewer
import ViewerMonad2()

import GuiClass
import SolutionTree hiding (makeModuleTree)
import qualified SolutionTree as S (makeModuleTree)

import GuiViewer.Class

import SearchMode
import DeclarationPath (SolutionPath(..))
import qualified DeclarationPath

import GuiError

import ErrorParser.Types

-- | User error
type UserError = GuiError

-- | Wrapper for all of the typeclasses needed to execute gui actions
type GuiCommand t m = 
    ( MonadTrans t
    , MonadSplice t
    , MonadUnsplice t
    , ViewerMonad m
    , SolutionClass m
    , PersistenceClass m
    , ProjectModuleClass m
    , ProjectExternModuleClass m
    , ModuleExportClass m
    , ModuleImportClass m
    , ModuleDeclarationClass m
    , ModulePragmaClass m
    , ExternModuleExportClass m
    , GuiViewerClass m
    , SolutionViewClass (t m)
    , EditorBufferClass (t m)
    , BuildBufferClass (t m)
    , SearchBarClass (t m)
    , ErrorListClass (t m)
    , Monad (t (SolutionResult UserError m))
    , ViewerStateClass m
    , ModuleLocationClass m
    , SolutionInitializerClass (t m)
    , ProjectInitializerClass (t m)
    , EnvironmentMonad m
    , PersistToken m ~ ViewerPersistToken m
    )

-- | Fail the current action
doError :: ( GuiCommand t m ) 
        => SolutionError UserError 
        -> t (SolutionResult UserError m) a
doError = lift . throwE 

-- | Start the process of creating a new solution
doNewStart :: ( GuiCommand t m
              )
           => t (SolutionResult UserError m) ()
doNewStart = splice setupSolutionCreator

-- | Start the process of creating a new project
doNewProjectStart :: ( GuiCommand t m
                     )
                  => t (SolutionResult UserError m) ()
doNewProjectStart = splice $ setupProjectCreator Nothing

-- | Start the process of editing a project
doEditProjectStart :: ( GuiCommand t m
                      , m ~ ClassProjectInitializerMonad (t m)
                      , Args (ProjectArgType m)
                      )
                   => ProjectInfo
                   -> t (SolutionResult UserError m) ()
doEditProjectStart pji = do
    retriever <- lift $ lift getProjectRetriever
    p <- lift $ runProjectRetriever retriever pji
    splice $ setupProjectCreator (Just p)

-- | Complete the process of creating a new project
doNew :: ( GuiCommand t m
         , MonadIO m
         , Args (ArgType m)
         )
      => Maybe FilePath
      -> String
      -> Maybe String
      -> t (SolutionResult UserError m) ()
doNew maybeSolutionRoot projectName templateName = do
    case maybeSolutionRoot of
        Nothing -> doError $ InvalidOperation "Please choose a directory" ""
        Just projectRoot -> do
            lift $ wrapIOError $ setCurrentDirectory projectRoot
            --lift $ setDirectoryToOpen $ projectRoot </> projectName
            initializer <- lift $ lift $ getInitializer
            let args = projectName : maybe [] (:[]) templateName
            r <- case runInitializerWithInput initializer args of
                Right x -> lift x
                Left err -> doError $ 
                    InternalError "Failed to parse initialization arguments" 
                                  err 
            case r of
                InitializerSucceeded _ _ tok -> do
                    --tok <- lift $ lift $ prepareDirectoryPathToken $ projectRoot </> projectName
                    lift $ load tok
                    populateTree
                    lift $ saveSolution $ Just $ projectRoot </> projectName
                InitializerFailed out err -> 
                    doError $ InvalidOperation (T.unpack $ out <> err) ""

-- | Open a solution
doOpen :: ( GuiCommand t m )
       => FilePath
       -> t (SolutionResult UserError m)  ()
doOpen path = do
    lift $ openSolution path
    populateTree

-- | Retrieve a declaration, module header, or unparsable module contents
openItem 
    :: ( GuiCommand t m
       )
    => SolutionPath
    -> t (SolutionResult UserError m) Text
openItem (DeclarationPath pji mi di) = do
    decl <- lift $ getDeclaration pji mi di
    splice $ setEditorBufferTextHighlighted $ body decl
    --lift $ liftIO $ print $ body decl
    lift $ lift $ setCurrentDecl pji mi di
    return $ body decl
openItem (ModulePath pji mi) = do
    header <- lift $ getModuleHeader pji mi
    splice $ setEditorBufferTextHighlighted header
    lift $ lift $ setCurrentModule pji mi
    return $ header
openItem (UnparsableModulePath pji mi) = do
    Just (contents, _, _) <- lift $ getUnparsableModule pji mi
    splice $ setEditorBufferTextHighlighted contents
    lift $ lift $ setCurrentModule pji mi
    return contents
openItem _ = do
    splice $ setEditorBufferText ""
    return ""

-- | Open a declaration or module header
doGetDecl :: ( GuiCommand t m )
          => TreePath
          -> t (SolutionResult UserError m)  ()
doGetDecl path = do
    index <- splice $ findAtPath path
    case index of
        DeclResult pji mi di -> do
            text <- openItem $ DeclarationPath pji mi di
            lift $ lift $ openDeclarationInHistory (DeclarationPath pji mi di) 
                                                   text
        ModuleResult pji mi True -> do
            text <- openItem $ ModulePath pji mi
            lift $ lift $ openDeclarationInHistory (ModulePath pji mi) text
        UnparsableModuleResult pji mi _ _ -> do
            text <- openItem $ UnparsableModulePath pji mi
            lift $ lift $ openDeclarationInHistory (UnparsableModulePath pji mi) text
        _ -> return ()

doGetItem :: ( GuiCommand t m )
          => SolutionPath
          -> t (SolutionResult UserError m) ()
doGetItem path = do
    text <- openItem path
    lift $ lift $ openDeclarationInHistory path text

doGotoSrcLoc :: ( GuiCommand t m )
             => SrcLoc
             -> t (SolutionResult UserError m) ()
doGotoSrcLoc SrcLoc{srcRow, srcColumn}
    = splice $ selectEditorBufferText (srcRow-1,srcColumn-1) 
                                      (srcRow-1,srcColumn-1)

-- | Build the current project
doBuild :: ( GuiCommand t m
           )
        => t (SolutionResult UserError m) ()
doBuild = do
    splice $ setBuildBufferText ""
    splice $ clearErrorList
    
    lift $ prepareBuild
    builder <- lift $ lift $ getBuilder
    result <- lift $ runBuilder builder
    let (text,errors) = case result of
            BuildSucceeded logText buildWarnings -> (logText, buildWarnings)
            BuildFailed logText buildErrors -> (logText, buildErrors)

    --   Group the errors into lists that have the same module, then batch fetch
    -- each group's offsets. 
    let sortedErrors = OMap.partitionBy errorLocation errors
    let errorSrcLoc e = SrcLoc (errorRow e) (errorColumn e)
    let fetchErrorLocations (ErrorLocation proj m) es = do
            let pji = ProjectInfo $ getProjectName proj
            let mi = ModuleInfo $ Symbol $ getModuleName m
            let mkItemPath = ProjectChild pji . ModuleChild mi
            let fixLocation e locResult = flip mapError e $ \_ _ _ msg -> case locResult of
                    Just (solutionItem, SrcLoc r' c')
                        -> ( mkItemPath $ Just solutionItem
                           , r'
                           , c'
                           , msg
                           )
                    Nothing
                        -> ( mkItemPath Nothing
                           , Row 0
                           , Column 0
                           , errorMessage e
                           )
            let locs = map errorSrcLoc es 
            locs' <- lift $ getModuleItemAtLocation pji mi locs
            return $ map (uncurry fixLocation) (zip es locs') 
    errors' <- liftM (concat . OMap.elems) 
             $ OMap.mapWithKeyM fetchErrorLocations sortedErrors

    -- Add the whole text to the log tab and populate the error tab
    splice $ setBuildBufferText text
    splice $ mapM_ addErrorToList errors'

-- | Run the current project
doRun :: ( GuiCommand t m
         )
      => t (SolutionResult UserError m) ()
doRun = do
    runner <- lift $ lift $ getRunner
    maybePji <- lift $ lift $ getCurrentProject
    case maybePji of
        Nothing -> doError $ InvalidOperation "No project selected" ""
        Just pji -> do
            r <- lift $ runRunner runner pji
            let text = case r of
                    RunSucceeded out err -> out <> err
                    RunFailed out err -> out <> err
            splice $ setBuildBufferText text


-- | Apply a monadic function to the contents of the editor buffer, update the
-- matching entry in the history, then save the solution
doWithEditorBuffer :: ( GuiCommand t m
                      )
                   => (Text -> t (SolutionResult UserError m) a )
                   -> t (SolutionResult UserError m) a
doWithEditorBuffer f = do
    text <- splice $ getEditorBufferText Nothing Nothing
    result <- f text
    lift $ lift $ replaceHistoryText text
    lift $ saveSolution Nothing
    return result

-- | Save a declaration from the editor buffer
saveDeclaration :: ( GuiCommand t m 
                   )
                => ProjectInfo
                -> ModuleInfo
                -> DeclarationInfo
                -> t (SolutionResult UserError m) ()
saveDeclaration pji mi di = do
    -- Take the editor buffer contents, parse it into a declaration, and
    -- update it in the backing
    di' <- doWithEditorBuffer $ \text -> do
        lift 
            $ editDeclaration pji mi di 
            $ const 
            $ Declaration.parseAndCombine text Nothing
    -- If the declaration's name has changed, update the tree with the
    -- new name, and replace the path in the history
    when (di /= di') $ do
        splice 
            $ updateSolutionTreeNode (DeclarationPath pji mi di) 
            $ const 
            $ DeclElem di'
        lift $ lift $ setCurrentDecl pji mi di'
        lift $ lift $ replaceHistoryPath $ DeclarationPath pji mi di'

-- | Save an unparsable module from the editor buffer
saveUnparsableModule :: ( GuiCommand t m
                        )
                     => ProjectInfo
                     -> ModuleInfo
                     -> t (SolutionResult UserError m) ()
saveUnparsableModule pji mi = do
    -- Update the text from the buffer, still marked as
    -- unparsable
    Just (_, loc, msg) <- lift $ getUnparsableModule pji mi
    doWithEditorBuffer $ \t -> do
        lift $ setModuleUnparsable pji mi t loc msg
    mi' <- lift $ refreshModule pji mi
    result' <- lift $ getUnparsableModule pji mi'
    case result' of
        -- If the module is now parsable
        Nothing -> do
            -- Create the tree for the module
            mTree <- lift $ M.makeModuleTree pji mi'
            let sTree = S.makeModuleTree mTree
            
            splice $ moveModuleInTree pji mi mi' sTree
            
            -- Update the history based on the change
            lift $ lift $ replaceHistoryPath (ModulePath pji mi)
            text <- openItem (ModulePath pji mi')
            lift $ lift $ replaceHistoryText text
            
            -- Replace the buffer contents with the module's header
            splice $ setEditorBufferTextHighlighted text
        -- If the module is still unparsable, do nothing
        _ -> return ()


-- | Save the a module header from the editor buffer
saveModuleHeader :: ( GuiCommand t m
                    )
                 => ProjectInfo
                 -> ModuleInfo
                 -> t (SolutionResult UserError m) ()
saveModuleHeader pji mi = doWithEditorBuffer $ \text -> do
    lift $ editModuleHeader pji mi $ const text

-- | Save a module header or an unparsable module from the editor buffer
saveModule :: ( GuiCommand t m
              )
           => ProjectInfo
           -> ModuleInfo
           -> t (SolutionResult UserError m) ()
saveModule pji mi = do
    result <- lift $ getUnparsableModule pji mi
    case result of
        -- If we're editing an unparsable module
        Just _ -> saveUnparsableModule pji mi
        -- If we're editing a module header, simply update the header
        -- field in the backing
        Nothing -> saveModuleHeader pji mi

-- | Save the current declaration/module header/unparsable module
doSave :: forall t m 
        . ( GuiCommand t m
          )
       => t (SolutionResult UserError m) ()
doSave = do 
    declResult <- lift $ lift $ getCurrentDeclaration
    modResult <- lift $ lift $ getCurrentModule
    case (declResult,modResult) of
        -- If we're editing a declaration
        (Just (pji, mi, di),_) -> saveDeclaration pji mi di
        -- If we're editing a module header or an unparsable module
        (_,Just (pji, mi)) -> saveModule pji mi
        _ -> return ()




-- | Save the entire solution
doSaveSolution :: ( GuiCommand t m
                  )
              => Maybe FilePath
              -> t (SolutionResult UserError m)  ()
doSaveSolution = lift . saveSolution

-- | Create a new solution
doAddSolution :: ( GuiCommand t m
                 , m ~ ClassSolutionInitializerMonad (t m)
                 , Args (ArgType m)
                 )
              => t (SolutionResult UserError m) ()
doAddSolution = do
    args <- unsplice $ getSolutionCreatorArg
    initializer <- lift $ lift $ getInitializer
    r <- lift $ runInitializer initializer args
    case r of
        InitializerSucceeded _ _ tok -> do
            lift $ load tok
            populateTree
        InitializerFailed out err -> do
            doError $ InvalidOperation (T.unpack $ out <> err) ""
    splice finalizeSolutionCreator

-- | Create a new project
doAddProject :: ( GuiCommand t m
                , m ~ ClassProjectInitializerMonad (t m)
                , Args (ProjectArgType m)
                )
             => t (SolutionResult UserError m) ()
doAddProject = do
    arg <- unsplice $ getProjectCreatorArg
    initializer <- lift $ lift getProjectInitializer
    result <- lift $ runProjectInitializer initializer arg
    case result of
        ProjectInitializerSucceeded _ _ pji -> do
            splice finalizeProjectCreator
            lift $ addProject pji
            splice $ insertSolutionTreeNode SolutionPath (ProjectElem pji)
            lift $ finalize Nothing
        ProjectInitializerFailed out err -> doError $ InvalidOperation (T.unpack $ out <> err) ""

doEditProject :: ( GuiCommand t m
                , m ~ ClassProjectInitializerMonad (t m)
                , Args (ProjectArgType m)
                )
             => ProjectInfo
             -> t (SolutionResult UserError m) ()
doEditProject pji = do
    arg <- unsplice $ getProjectCreatorArg
    editor <- lift $ lift getProjectEditor
    result <- lift $ runProjectEditor editor pji arg
    case result of
        ProjectEditorSucceeded _ _ pji' -> do
            splice finalizeProjectCreator
            lift $ editProjectInfo pji (const pji')
            splice $ updateSolutionTreeNode (ProjectPath pji) (const $ ProjectElem pji')
            lift $ finalize Nothing
        ProjectEditorFailed out err -> doError $ InvalidOperation (out ++ err) ""

-- | Add a module to a project
doAddModule :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> t (SolutionResult UserError m)  ()
doAddModule pji mi = do
    lift $ createModule pji mi
    
    mTree <- lift $ M.makeModuleTree pji mi
    let sTree = S.makeModuleTree mTree
    
    splice $ addModuleToTree pji mi sTree
    

-- | Remove a module from a project
doRemoveModule :: ( GuiCommand t m )
               => ProjectInfo
               -> ModuleInfo
               -> t (SolutionResult UserError m)  ()
doRemoveModule pji mi = do
    lift $ removeModule pji mi
    splice $ removeModuleFromTree pji mi
    

-- | Add a declaration to a module
doAddDeclaration :: ( GuiCommand t m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> t (SolutionResult UserError m)  ()
doAddDeclaration pji mi di = do
    let newdecl = WithBody (UnparseableDeclaration di) ""
    lift $ addDeclaration pji mi newdecl
    splice $ insertSolutionTreeNode (ModulePath pji mi) $ DeclElem di
    decl <- lift $ getDeclaration pji mi di
    splice $ setEditorBufferTextHighlighted $ body decl
    lift $ lift $ setCurrentDecl pji mi di
    lift 
        $ lift 
        $ openDeclarationInHistory (DeclarationPath pji mi di)
        $ body decl

-- | Remove a declaration from a module
doRemoveDeclaration :: ( GuiCommand t m )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> t (SolutionResult UserError m)  ()
doRemoveDeclaration pji mi di = do
    lift $ removeDeclaration pji mi di
    splice $ removeSolutionTreeNode $ DeclarationPath pji mi di

-- | Change the export list of a module as to not export a declaration.
-- This may fail if the export is not explicitly exported, or if the
-- declaration is provided by more than one export
doUnExportDeclaration :: ( GuiCommand t m )
                      => ProjectInfo
                      -> ModuleInfo
                      -> DeclarationInfo
                      -> t (SolutionResult UserError m)  ()
doUnExportDeclaration pji mi (SymbolDeclarationInfo sym) = do
    matches <- lift $ do
        es <- do
            maybeEis <- getExports pji mi
            case maybeEis of
                Nothing -> throwE 
                         $ InvalidOperation "All symbols are exported" ""
                Just eis -> 
                    forM eis $ \ei -> do
                        (WithBody e _) <- getExport pji mi ei
                        syms <- Export.symbolsProvided' pji mi e
                        return (ei,syms)
        return $ flip filter es $ \(_,syms) -> sym `elem` syms
    case matches of
        [] -> doError $ SymbolNotExported mi sym ""
        [(ei,syms)] -> do
            case syms of
                [] -> doError $ InvalidOperation "Internal Error" 
                                                 "doUnExportDeclaration"
                [_] -> do
                    lift $ removeExport pji mi ei
                    splice $ removeSolutionTreeNode $ ExportPath pji mi ei
                _ -> doError $ Unsupported 
                             $ "Symbol is exported with other symbols, "
                               ++ "please remove export manually"
        _ -> doError $ Unsupported 
                       "Multiple exports found, please remove exports manually"
doUnExportDeclaration _ _ _ = doError $ InvalidOperation "This declaration is not exportable" ""


doMoveDeclaration :: ( GuiCommand t m )
                  => ProjectInfo
                  -> ModuleInfo
                  -> DeclarationInfo
                  -> ProjectInfo
                  -> ModuleInfo
                  -> t (SolutionResult UserError m) ()
doMoveDeclaration pji mi di pji' mi' = do
    lift $ do
        decl <- getDeclaration pji mi di
        removeDeclaration pji mi di
        addDeclaration pji' mi' decl
    splice $ do
        removeSolutionTreeNode (DeclarationPath pji mi di)
        insertSolutionTreeNode (ModulePath pji' mi') (DeclElem di)
    lift $ lift $ do
        updateHistoryPath (DeclarationPath pji mi di) 
                          (DeclarationPath pji' mi' di)
        cdi <- getCurrentDeclaration
        when (cdi == Just (pji, mi, di)) $ do
            setCurrentDecl pji' mi' di
            

-- | An an import to a module
doAddImport :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> Text
            -> t (SolutionResult UserError m)  (Maybe (SolutionError UserError))
doAddImport pji mi importStr = do
    case Import.parse importStr of
        Right newImport -> do
            let i = WithBody newImport importStr
            ii <- lift $ addImport pji mi i
            splice
                $ insertSolutionTreeNode (ImportsPath pji mi) 
                $ ImportElem ii i
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> doError err

-- | Remove an import from a module
doRemoveImport :: ( GuiCommand t m )
               => ProjectInfo
               -> ModuleInfo
               -> ImportId
               -> t (SolutionResult UserError m) ()
doRemoveImport pji mi ii = do
    lift $ removeImport pji mi ii
    splice $ removeSolutionTreeNode $ ImportPath pji mi ii

-- | Retrieve the body of an import
doGetImport :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> ImportId
            -> t (SolutionResult UserError m)  (Maybe Text)
doGetImport pji mi ii = do
    (WithBody _ b) <- lift $ getImport pji mi ii
    return $ Just b

-- | Modify an existing import
doEditImport :: ( GuiCommand t m )
             => ProjectInfo
             -> ModuleInfo
             -> ImportId
             -> Text
             -> t (SolutionResult UserError m) (Maybe (SolutionError UserError))
doEditImport pji mi ii importStr = do
    case Import.parse importStr of
        Right newImport -> do
            lift $ removeImport pji mi ii
            let i' = WithBody newImport importStr
            ii' <- lift $ addImport pji mi i'
            splice 
                $ updateSolutionTreeNode (ImportPath pji mi ii) 
                $ const 
                $ ImportElem ii' i'
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> doError err

-- | Add an export to a module
doAddExport :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> Text
            -> t (SolutionResult UserError m) (Maybe (SolutionError UserError))
doAddExport pji mi exportStr = do
    case Export.parse exportStr of
        Right newExport -> do
            let e = WithBody newExport exportStr
            ei <- lift $ addExport pji mi e
            splice 
                $ insertSolutionTreeNode (ExportsPath pji mi) 
                $ ExportElem ei e
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> doError err

-- | Remove an export from a module
doRemoveExport :: ( GuiCommand t m )
               => ProjectInfo
               -> ModuleInfo
               -> ExportId
               -> t (SolutionResult UserError m)  ()
doRemoveExport pji mi ei = do
    lift $ removeExport pji mi ei
    splice $ removeSolutionTreeNode $ ExportPath pji mi ei

-- | Retrieve the body of an export
doGetExport :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> ExportId
            -> t (SolutionResult UserError m)  (Maybe Text)
doGetExport pji mi ei = do
    (WithBody _ b) <- lift $ getExport pji mi ei
    return $ Just b

-- | Modfiy an existing export
doEditExport :: ( GuiCommand t m )
             => ProjectInfo
             -> ModuleInfo
             -> ExportId
             -> Text
             -> t (SolutionResult UserError m) (Maybe (SolutionError UserError))
doEditExport pji mi ei exportStr = do
    case Export.parse exportStr of
        Right newExport -> do
            lift $ removeExport pji mi ei
            let e' = WithBody newExport exportStr
            ei' <- lift $ addExport pji mi e'
            splice 
                $ updateSolutionTreeNode (ExportPath pji mi ei) 
                $ const 
                $ ExportElem ei' e'
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> doError err

-- | Set a module to export everything
doExportAll :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> t (SolutionResult UserError m)  ()
doExportAll pji mi = do
    eis <- lift $ getExports pji mi
    splice $ forM_ (maybe [] id eis) $ \ei -> do
        removeSolutionTreeNode $ ExportPath pji mi ei
    lift $ exportAll pji mi
    
-- | Perform a text search or a declaration navigation depending on the state
-- of the search component.
doSearch :: ( GuiCommand t m 
            )
         => t (SolutionResult UserError m) ()
doSearch = do
    mode <- lift $ lift $ getSearchMode
    s <- splice getSearchBarText
    case mode of
        Find -> do 
            (start,end) <- splice getEditorBufferCursor
            textBefore <- splice $ getEditorBufferText (Just start) Nothing
            textAfter <- splice $ getEditorBufferText Nothing (Just end)
            let search substr supstr = case T.splitOn supstr substr of
                    [] -> Nothing
                    (t:_) -> Just $ T.length t
            result <- return $ case (search s textAfter, search s textBefore) of
                (Just offset,_) -> Just offset
                (_,Just offset) -> Just offset
                _ -> Nothing
            case result of
                Just offset -> do
                    highlightStart <- splice 
                        $ getEditorBufferPositionAtIndex offset
                    highlightEnd <- splice 
                        $ getEditorBufferPositionAtIndex 
                        $ offset + T.length s
                    splice $ selectEditorBufferText highlightStart highlightEnd
                _ -> doError $ UserError $ TempError "Not Found"
        Navigate -> do
            case DeclarationPath.parse (T.unpack s) of
                Nothing -> doError $ UserError $ TempError "Not Found"
                Just dpath -> do
                    text <- openItem dpath
                    lift $ lift $ openDeclarationInHistory dpath text
                            
-- | Set the mode of the search component
doSetSearchMode 
    :: ( GuiCommand t m
       )
    => SearchMode
    -> t (SolutionResult UserError m) ()
doSetSearchMode mode = lift $ lift $ setSearchMode mode

-- | Get the word that contains a start and end offset
getCurrentWord :: Text -> Int -> Int -> (Char -> Bool) -> Text
getCurrentWord text startOffset endOffset isChar = 
    nextPreStart 
    <> between 
    <> nextPostEnd
  where
    preStart = T.take startOffset text
    postEnd = T.drop endOffset text
    between = T.take (endOffset - startOffset) $ T.drop startOffset text
    nextPreStart = T.reverse $ T.takeWhile isChar $ T.reverse preStart
    nextPostEnd = T.takeWhile isChar $ postEnd

-- | Find the symbol the cursor is over, then open the declaration that created
-- that symbol
doGotoDeclaration
    :: ( GuiCommand t m
       )
    => t (SolutionResult UserError m) ()
doGotoDeclaration = do
    text <- splice $ getEditorBufferText Nothing Nothing
    (startPos,endPos) <- splice $ getEditorBufferCursor
    startOffset <- splice $ getEditorBufferIndexAtPosition startPos
    endOffset <- splice $ getEditorBufferIndexAtPosition endPos
    let currentWord = getCurrentWord text startOffset endOffset isChar
        isIdentChar c = isLower c || isUpper c || c == '_' || c == '\''
        isSymChar c = c `telem` "!@#$%^&*:.<-=>|"
        firstIs f = startOffset < T.length text 
                 && f (text `T.index` startOffset) 
        lastIs f = (endOffset < T.length text && f (text `T.index` startOffset))
                      || (not (T.null text) && f (T.last text))
        isChar 
            | firstIs isIdentChar && lastIs isIdentChar = isIdentChar
            | firstIs isSymChar && lastIs isSymChar = isSymChar
            | otherwise = const False
        c `telem` t = isJust $ T.findIndex (==c) t
    hits <- lift $ Solution.findSymbol $ Symbol currentWord
    let hits' = join . fmap (sequenceA . fmap (join .  fmap sequenceA)) $ hits
    case hits' of
        [(ProjectChild pji (ModuleChild mi di))] -> do
            declText <- openItem $ DeclarationPath pji mi di
            lift 
                $ lift 
                $ openDeclarationInHistory (DeclarationPath pji mi di) declText
        _ -> return ()

-- | Go to the previous declaration in history
doBackHistory
    :: ( GuiCommand t m
       )
    => t (SolutionResult UserError m) ()
doBackHistory = do
    text <- splice $ getEditorBufferText Nothing Nothing
    lift $ lift $ replaceHistoryText text
    result <- lift $ lift $ navigateHistoryBack
    case result of
        Just (path,text') -> do
            splice $ setEditorBufferTextHighlighted text'
            lift $ lift $ case path of
                DeclarationPath pji mi di -> setCurrentDecl pji mi di
                ModulePath pji mi -> setCurrentModule pji mi
                ProjectPath pji -> setCurrentProject pji
                _ -> setNoCurrentDecl
        Nothing -> return ()

-- | Go to the next declaration in history
doForwardHistory
    :: ( GuiCommand t m
       )
    => t (SolutionResult UserError m) ()
doForwardHistory = do
    text <- splice $ getEditorBufferText Nothing Nothing
    lift $ lift $ replaceHistoryText text
    result <- lift $ lift $ navigateHistoryForward
    case result of
        Just (path,text') -> do
            splice $ setEditorBufferTextHighlighted text'
            lift $ lift $ case path of
                DeclarationPath pji mi di -> setCurrentDecl pji mi di
                ModulePath pji mi -> setCurrentModule pji mi
                ProjectPath pji -> setCurrentProject pji
                _ -> setNoCurrentDecl
        Nothing -> return ()

-- | Jump to the location of an error
doJumpToErrorLocation 
    :: ( GuiCommand t m 
       )
    => TreePath
    -> t (SolutionResult UserError m) Bool
doJumpToErrorLocation [ix] = do
    err <- splice $ getErrorAtIndex ix
    let row = errorRow err
    let col = errorColumn err
    let ProjectChild pji (ModuleChild mi maybeItem) = errorLocation err
    case maybeItem of
        Just (DeclarationString di') -> do
            let di = item di'
            text <- openItem $ DeclarationPath pji mi di
            lift 
                $ lift 
                $ openDeclarationInHistory (DeclarationPath pji mi di) text
            splice $ selectEditorBufferText (row, col - 1) (row, col - 1)
            return True
        _ -> return False
doJumpToErrorLocation _ = doError $ InternalError "Invalid Error Tree Path" ""

