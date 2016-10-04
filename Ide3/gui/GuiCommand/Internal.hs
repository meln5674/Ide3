{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module GuiCommand.Internal where

import Ide3.OrderedMap (OrderedMap)
import qualified Ide3.OrderedMap as OMap

import Data.Proxy

import Data.Char
import Data.Tree
import Data.List

import System.Directory
import System.FilePath

import Control.Monad.Catch

import Control.Concurrent

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict (gets)

import Ide3.Types
import Ide3.Utils
import Ide3.NewMonad

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Project as Project
import qualified Ide3.Solution as Solution (findSymbol)
import qualified Ide3.Import as Import
import qualified Ide3.Export as Export

import qualified Ide3.ModuleTree as M (makeModuleTree)

import Builder
import Initializer
import Runner

import EnvironmentMonad

import Viewer
import ViewerMonad2

import GuiClass
import SolutionTree hiding (makeModuleTree)
import qualified SolutionTree as S (makeModuleTree)

import GuiViewer.Class

import SearchMode
import DeclarationPath (SolutionPath(..))
import qualified DeclarationPath

import Args

import GuiError

import ErrorParser.Types

type UserError = GuiError

{-
type DialogOnErrorArg t proxy m p  a
    = GuiEnvT {-proxy-} m p  (SolutionResult (GuiViewer (ViewerStateT m)) UserError) a
-}

{-
newtype DialogOnErrorArg t m a = DialogOnErrorArg 
    { runDialogOnErrorArg :: t (SolutionResult (GuiViewer (ViewerStateT m)) UserError) a
    }

instance MonadTrans t => MonadTrans (DialogOnErrorArg t) where
    lift = DialogOnErrorArg . lift . lift . lift . lift

deriving instance (MonadTrans t, SolutionViewClass m) => SolutionViewClass (DialogOnErrorArg t m)

  
type GuiCommand t m p 
    = ( MonadTrans t
      , Monad m
      {-, Monad (t m)
      , Monad (t (SolutionResult m UserError))
      , Monad (t (SolutionResult (GuiViewer m) UserError))
      , Monad (t (SolutionResult (GuiViewer (ViewerStateT m)) UserError))-}
      , ViewerMonad m
      --, InteruptMonad2 p m
      , SolutionClass m
      , PersistenceClass m
      , ProjectModuleClass m
      , ProjectExternModuleClass m
      , ModuleExportClass m
      , ModuleImportClass m
      , ModuleDeclarationClass m
      , ModulePragmaClass m
      , ExternModuleExportClass m
      )
-}

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
    , GuiViewerClass (t m)
    , ViewerStateClass m
    , ModuleLocationClass m
--    , ProjectInitializerClass' (t m) m
--    , SolutionInitializerClass' (t m) m
    , SolutionInitializerClass (t m)
    , EnvironmentMonad m
    )

doError :: ( GuiCommand t m ) => SolutionError UserError -> t (SolutionResult UserError m) a
doError = lift . throwE 


doNewStart :: ( GuiCommand t m
              )
           => t (SolutionResult UserError m) ()
doNewStart = splice setupSolutionCreator
    


doNew :: ( GuiCommand t m
         , MonadIO m
         , InitializerMonad m
         , Args (ArgType m)
         )
      => Maybe FilePath
      -> String
      -> Maybe String
      -> t (SolutionResult UserError m) ()
doNew maybeSolutionRoot projectName templateName = do
    case maybeSolutionRoot of
        Nothing -> lift $ throwE $ InvalidOperation "Please choose a directory" ""
        Just projectRoot -> do
            lift $ wrapIOError $ setCurrentDirectory projectRoot
            lift $ setDirectoryToOpen $ projectRoot </> projectName
            initializer <- lift $ lift $ getInitializer
            let args = case templateName of
                    Just templateName -> [projectName, templateName]
                    Nothing -> [projectName]
            r <- case runInitializerWithInput initializer args of
                Right x -> lift x
                Left err -> lift $ throwE $ InternalError "Failed to parse initialization arguments" err 
            case r of
                InitializerSucceeded{} -> do
                    lift $ load
                    populateTree
                    lift $ saveSolution $ Just $ projectRoot </> projectName
                InitializerFailed out err -> lift $ throwE $ InvalidOperation (out ++ err) ""

doOpen :: ( GuiCommand t m, MonadIO m )
       => FilePath
       -> t (SolutionResult UserError m)  ()
doOpen path = do
    lift $ openSolution path
    populateTree


openItem 
    :: ( GuiCommand t m
       )
    => SolutionPath
    -> t (SolutionResult UserError m) String
openItem (DeclarationPath pji mi di) = do
    decl <- lift $ getDeclaration pji mi di
    splice $ setEditorBufferTextHighlighted $ body decl
    lift $ lift $ setCurrentDecl pji mi di
    return $ body decl
openItem (ModulePath pji mi) = do
    header <- lift $ getModuleHeader pji mi
    splice $ setEditorBufferTextHighlighted header
    lift $ lift $ setCurrentModule pji mi
    return $ header
openItem _ = do
    splice $ setEditorBufferText ""
    return ""

doGetDecl :: ( GuiCommand t m )
          => TreePath
          -> t (SolutionResult UserError m)  ()
doGetDecl path = do --withGuiComponents $ \comp -> lift $ do
    --index <- wrapIOError $ withSolutionTree comp $ findAtPath path
    index <- splice $ findAtPath path
    --openDecls <- lift $ lift $ getOpenDeclarations
    --lift $ lift $ mapM_ closeDeclaration openDecls
    case index of
        DeclResult pi mi di -> do
            text <- openItem $ DeclarationPath pi mi di
            lift $ lift $ openDeclarationInHistory (DeclarationPath pi mi di) text
        ModuleResult pi mi True -> do
            text <- openItem $ ModulePath pi mi
            lift $ lift $ openDeclarationInHistory (ModulePath pi mi) text
        _ -> return ()

doBuild :: ( GuiCommand t m
           , MonadMask m
           , MonadIO m
           , BuilderMonad m
           )
        => t (SolutionResult UserError m) ()
doBuild = do --withGuiComponents $ \comp -> lift $ do
    lift $ liftIO $ putStrLn "Clearing error list"
    splice $ setBuildBufferText ""
    splice $ clearErrorList
    
    lift $ liftIO $ putStrLn "Building"
    lift $ prepareBuild
    builder <- lift $ lift $ getBuilder
    r <- lift $ runBuilder builder
    let (text,errors) = case r of
            BuildSucceeded log warnings -> (log,warnings)
            BuildFailed log errors -> (log,errors)

    lift $ liftIO $ putStrLn "Fetching error locations"
    
    --   Group the errors into lists that have the same module, then batch fetch
    -- each group's offsets. 
    let sortedErrors = OMap.partitionBy errorLocation errors
    let errorSrcLoc e = SrcLoc (errorRow e) (errorColumn e)
    let fetchErrorLocations (ErrorLocation proj mod) es = do
            let pji = ProjectInfo $ getProjectName proj
            let mi = ModuleInfo $ Symbol $ getModuleName mod
            let mkItemPath = ProjectChild pji . ModuleChild mi
            let fixLocation e result = flip mapError e $ \_ _ _ msg -> case result of
                    Just (item, SrcLoc r' c') -> (mkItemPath $ Just item, r', c', msg)
                    Nothing -> (mkItemPath Nothing, Row 0, Column 0, errorMessage e)
            let locs = map errorSrcLoc es 
            locs' <- lift $ getModuleItemAtLocation pji mi locs
            return $ map (uncurry fixLocation) (zip es locs') 
    errors' <- liftM (concat . OMap.elems) $ OMap.mapWithKeyM fetchErrorLocations sortedErrors

    lift $ liftIO $ putStrLn "Setting log text"
    splice $ setBuildBufferText text

    lift $ liftIO $ putStrLn $ "Adding errors (" ++ show (length errors') ++ ")"
    splice $ mapM_ addErrorToList errors'

doRun :: ( GuiCommand t m
         , MonadMask m
         , MonadIO m
         , RunnerMonad m
         )
      => t (SolutionResult UserError m) ()
doRun = do --withGuiComponents $ \comp -> lift $ do
    runner <- lift $ lift $ getRunner
    r <- lift $ runRunner runner
    let text = case r of
            RunSucceeded out err -> out ++ err
            RunFailed out err -> out ++ err
    --wrapIOError $ withBuildBuffer comp $ flip textBufferSetText text
    splice $ setBuildBufferText text


doSave :: forall ia pia t m
        . ( GuiCommand t m
          , MonadMask m
          )
       => t (SolutionResult UserError m) ()
doSave = do --withGuiComponents $ \comp -> lift $ do
    let doWithBuffer :: (String -> t (SolutionResult UserError m) a )
                     -> t (SolutionResult UserError m) a
        doWithBuffer f = do
            text <- splice $ getEditorBufferText Nothing Nothing
            result <- f text
            lift $ lift $ replaceHistoryText text
            lift $ saveSolution Nothing
            return result
    declResult <- lift $ lift $ getCurrentDeclaration
    modResult <- lift $ lift $ getCurrentModule
    case (declResult,modResult) of
        (Just (pi, mi, di),_) -> do
            di' <- doWithBuffer $ \text -> do
                lift $ editDeclaration pi mi di $ const $ Declaration.parseAndCombine text Nothing
            when (di /= di') $ do
                splice $ updateSolutionTreeNode (DeclarationPath pi mi di) $ const $ DeclElem di'
                lift $ lift $ setCurrentDecl pi mi di'
                lift $ lift $ replaceHistoryPath $ DeclarationPath pi mi di'
        (_,Just (pi, mi)) -> doWithBuffer $
                lift . editModuleHeader pi mi . const
        _ -> return ()
        

doSaveSolution :: ( GuiCommand t m
                  , MonadMask m
                  )
              => Maybe FilePath
              -> t (SolutionResult UserError m)  ()
doSaveSolution = lift . saveSolution


doAddSolution :: ( GuiCommand t m
                 , m ~ ClassSolutionInitializerMonad (t m)
                 , Args (ArgType m)
                 , InitializerMonad m
                 )
              => t (SolutionResult UserError m) ()
doAddSolution = do
    args <- unsplice $ getSolutionCreatorArg
    initializer <- lift $ lift $ getInitializer
    r <- lift $ runInitializer initializer args
    case r of
        InitializerSucceeded{} -> do
            lift $ load
            populateTree
        InitializerFailed out err -> do
            lift $ throwE $ InvalidOperation (out ++ err) ""
    splice finalizeSolutionCreator


doAddProject :: ( GuiCommand t m
                )
             => t (SolutionResult UserError m) ()
doAddProject = do
    {-
    arg <- unsplice $ getProjectCreatorArg
    initializer <- lift $ lift $ getProjectInitializer
    lift $ runProjectInitializer initializer arg
    splice finalizeProjectCreator
    -}
    return ()

doAddModule :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> t (SolutionResult UserError m)  ()
doAddModule pi mi = do
    
    -- Add the module to the project
    lift $ createModule pi mi
    
    -- Create a tree structure for the module and its parent modules as if we
    -- were populating the tree
    mTree <- lift $ M.makeModuleTree pi mi
    let sTree = S.makeModuleTree mTree
    
    --   We now have a tree rooted at the ancestor of the ancestor of the new
    -- module new one, and each of the trees have a single branch, pointing to
    -- the next ancestor, until it reaches the node for the new module.
    
    --   Because there may be orgnaizational nodes that need to be added, we
    -- traverse down the tree, checking the stored tree if there is already a
    -- node with that path. If there is, we go to the next descendent.
    --   Once we reach a node that is not present, that is the top of the tree
    -- that contains the new module, so we insert that tree wholesale.
    let loop parent tree@(Node (ModuleElem mi' _) trees') = do
            result <- lookupAtSolutionPath $ ModulePath pi mi'
            case (result, trees') of
                (Just _, [tree']) -> loop (Just mi') tree'
                (Nothing,_) -> insertSolutionTreeTree parentPath tree
          where
            parentPath = case parent of
                Just parent -> ModulePath pi parent
                Nothing -> ProjectPath pi
    splice $ loop Nothing sTree

doRemoveModule :: ( GuiCommand t m )
               => ProjectInfo
               -> ModuleInfo
               -> t (SolutionResult UserError m)  ()
doRemoveModule pji mi = do
    lift $ removeModule pji mi
    splice $ removeSolutionTreeNode $ ModulePath pji mi

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
    splice $ openDeclarationInHistory (DeclarationPath pji mi di) $ body decl

doRemoveDeclaration :: ( GuiCommand t m )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> t (SolutionResult UserError m)  ()
doRemoveDeclaration pji mi di = do
    lift $ removeDeclaration pji mi di
    splice $ removeSolutionTreeNode $ DeclarationPath pji mi di

doUnExportDeclaration :: ( GuiCommand t m )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> t (SolutionResult UserError m)  ()
doUnExportDeclaration pi mi (DeclarationInfo sym) = do
    matches <- lift $ do
        es <- do
            --m <- getModule pi mi
            maybeEis <- getExports pi mi
            case maybeEis of
                Nothing -> throwE $ InvalidOperation "All symbols are exported" ""
                Just eis -> 
                    forM eis $ \ei -> do
                        (WithBody e _) <- getExport pi mi ei
                        syms <- Export.symbolsProvided' pi mi e
                        return (ei,syms)
        return $ flip filter es $ \(_,syms) -> sym `elem` syms
    case matches of
        [] -> lift $ throwE $ SymbolNotExported mi sym ""
        [(ei,syms)] -> do
            case syms of
                [] -> doError $ InvalidOperation "Internal Error" "doUnExportDeclaration"
                [_] -> do
                    lift $ removeExport pi mi ei
                    splice $ removeSolutionTreeNode $ ExportPath pi mi ei
                _ -> doError $ Unsupported "Symbol is exported with other symbols, please remove export manually"
        _ -> doError $ Unsupported "Multiple exports found, please remove exports manually"

doAddImport :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> String
            -> t (SolutionResult UserError m)  (Maybe (SolutionError UserError))
doAddImport pi mi importStr = do
    case Import.parse importStr of
        Right newImport -> do
            let i = WithBody newImport importStr
            ii <- lift $ addImport pi mi i
            splice $ insertSolutionTreeNode (ImportsPath pi mi) $ ImportElem ii i
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err

doRemoveImport :: ( GuiCommand t m )
               => ProjectInfo
               -> ModuleInfo
               -> ImportId
               -> t (SolutionResult UserError m) ()
doRemoveImport pi mi ii = do
    lift $ removeImport pi mi ii
    splice $ removeSolutionTreeNode $ ImportPath pi mi ii

doGetImport :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> ImportId
            -> t (SolutionResult UserError m)  (Maybe String)
doGetImport pi mi ii = do
    (WithBody _ b) <- lift $ getImport pi mi ii
    return $ Just b

doEditImport :: ( GuiCommand t m )
             => ProjectInfo
             -> ModuleInfo
             -> ImportId
             -> String
             -> t (SolutionResult UserError m) (Maybe (SolutionError UserError))
doEditImport pi mi ii importStr = do
    case Import.parse importStr of
        Right newImport -> do
            lift $ removeImport pi mi ii
            let i' = WithBody newImport importStr
            ii' <- lift $ addImport pi mi i'
            splice $ updateSolutionTreeNode (ImportPath pi mi ii) $ const $ ImportElem ii' i'
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err

doAddExport :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> String
            -> t (SolutionResult UserError m) (Maybe (SolutionError UserError))
doAddExport pi mi exportStr = do
    case Export.parse exportStr of
        Right newExport -> do
            let e = WithBody newExport exportStr
            ei <- lift $ addExport pi mi e
            splice $ insertSolutionTreeNode (ExportsPath pi mi) $ ExportElem ei e
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err

doRemoveExport :: ( GuiCommand t m )
               => ProjectInfo
               -> ModuleInfo
               -> ExportId
               -> t (SolutionResult UserError m)  ()
doRemoveExport pi mi ei = do
    lift $ removeExport pi mi ei
    splice $ removeSolutionTreeNode $ ExportPath pi mi ei

doGetExport :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> ExportId
            -> t (SolutionResult UserError m)  (Maybe String)
doGetExport pi mi ei = do
    (WithBody _ b) <- lift $ getExport pi mi ei
    return $ Just b

doEditExport :: ( GuiCommand t m )
             => ProjectInfo
             -> ModuleInfo
             -> ExportId
             -> String
             -> t (SolutionResult UserError m)  (Maybe (SolutionError UserError))
doEditExport pi mi ei exportStr = do
    case Export.parse exportStr of
        Right newExport -> do
            lift $ removeExport pi mi ei
            let e' = WithBody newExport exportStr
            ei' <- lift $ addExport pi mi e'
            splice $ updateSolutionTreeNode (ExportPath pi mi ei) $ const $ ExportElem ei' e'
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err

doExportAll :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> t (SolutionResult UserError m)  ()
doExportAll pi mi = do
    eis <- lift $ getExports pi mi
    splice $ forM (maybe [] id eis) $ removeSolutionTreeNode . ExportPath pi mi
    lift $ exportAll pi mi
    

doSearch :: ( GuiCommand t m 
            )
         => t (SolutionResult UserError m) ()
doSearch = do
    mode <- lift $ lift $ getSearchMode
    s <- --withGuiComponents $ flip withSearchBuffer $ \buffer -> lift $ do
        --liftIO $ get buffer entryBufferText
        splice getSearchBarText
    case mode of
        Find -> do --withGuiComponents $ flip withEditorBuffer $ \buffer -> lift $ do
            --start <- liftIO $ textBufferGetStartIter buffer
            --end <- liftIO $ textBufferGetEndIter buffer
            --cursorMark <- liftIO $ textBufferGetInsert buffer
            --cursor <- liftIO $ textBufferGetIterAtMark buffer cursorMark
            (start,end) <- splice getEditorBufferCursor
            --textBefore <- liftIO $ textBufferGetText buffer start cursor False
            textBefore <- splice $ getEditorBufferText (Just start) Nothing
            --textAfter <- liftIO $ textBufferGetText buffer cursor end False
            textAfter <- splice $ getEditorBufferText Nothing (Just end) 
            let search substr supstr = go supstr 0
                  where
                    go supstr@(_:xs) i
                      | length substr > length supstr = Nothing
                      | substr `isPrefixOf` supstr = Just i
                      | otherwise = go xs (i+1)
            result <- return $ case (search s textAfter, search s textBefore) of
                (Just offset,_) -> Just offset
                (_,Just offset) -> Just offset
                _ -> Nothing
            case result of
                Just offset -> do
                    --highlightStart <- liftIO $ textBufferGetIterAtOffset buffer offset
                    highlightStart <- splice $ getEditorBufferPositionAtIndex offset
                    --highlightEnd <- liftIO $ textBufferGetIterAtOffset buffer $ offset + length s
                    highlightEnd <- splice $ getEditorBufferPositionAtIndex $ offset + length s
                    --liftIO $ textBufferSelectRange buffer highlightStart highlightEnd
                    splice $ selectEditorBufferText highlightStart highlightEnd
                _ -> lift $ throwE $ UserError $ TempError "Not Found"
        Navigate -> do
            case DeclarationPath.parse s of
                Nothing -> lift $ throwE $ UserError $ TempError "Not Found"
                Just dpath -> do
                    text <- openItem dpath
                    splice $ openDeclarationInHistory dpath text
                            

doSetSearchMode 
    :: ( GuiCommand t m
       )
    => SearchMode
    -> t (SolutionResult UserError m) ()
doSetSearchMode mode = lift $ lift $ setSearchMode mode

getCurrentWord :: String -> Int -> Int -> (Char -> Bool) -> String
getCurrentWord text startOffset endOffset isChar = nextPreStart ++ between ++ nextPostEnd
  where
    preStart = take startOffset text
    postEnd = drop endOffset text
    between = take (endOffset - startOffset) $ drop startOffset text
    nextPreStart = reverse $ takeWhile isChar $ reverse preStart
    nextPostEnd = takeWhile isChar $ postEnd

doGotoDeclaration
    :: ( GuiCommand t m
       )
    => t (SolutionResult UserError m) ()
doGotoDeclaration = do
    text <- splice $ getEditorBufferText Nothing Nothing
    (startPos,endPos) <- splice $ getEditorBufferCursor
    startOffset <- splice $ getEditorBufferIndexAtPosition startPos
    endOffset <- splice $ getEditorBufferIndexAtPosition endPos
    {-when (startOffset >= length text || endOffset >= length text)
        $ lift $ throwE $ UserError $ TempError $ show (startOffset, endOffset, length text)-}
    let currentWord = getCurrentWord text startOffset endOffset isChar
        isIdentChar c = isLower c || isUpper c || c == '_' || c == '\''
        isSymChar c = c `elem` "!@#$%^&*:.<-=>|"
        firstIs f  = startOffset < length text && f (text !! startOffset) 
        lastIs f = (endOffset < length text && f (text !! startOffset))
                      || (not (null text) && f (last text))
        isChar 
            | firstIs isIdentChar && lastIs isIdentChar = isIdentChar
            | firstIs isSymChar && lastIs isSymChar = isSymChar
            | otherwise = const False
    --lift $ throwE $ UserError $ TempError $ currentWord
    hits <- lift $ Solution.findSymbol $ Symbol $ currentWord
    let hits' = join . fmap (sequenceA . fmap (join .  fmap sequenceA)) $ hits
    case hits' of
        [] -> return ()
        [(ProjectChild pji (ModuleChild mi di))] -> do
            text <- openItem $ DeclarationPath pji mi di
            lift $ lift $ openDeclarationInHistory (DeclarationPath pji mi di) text
        (x:xs) -> return ()

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
        Nothing -> return ()

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
        Nothing -> return ()
