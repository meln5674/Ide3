{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module GuiCommand.Internal where

import Data.Proxy

import Data.Char
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

import Builder
import Initializer
import Runner

import EnvironmentMonad

import Viewer
import ViewerMonad2

import GuiClass
--import GuiEnv
--import GuiMonad
import SolutionTree

import GuiViewer.Class

import SearchMode
import DeclarationPath (DeclarationPath(..))
import qualified DeclarationPath

import Args

import GuiError

import ErrorParser.Types

type UserError = GuiError

{-
type DialogOnErrorArg t proxy m p  a
    = GuiEnvT proxy m p  (SolutionResult (GuiViewer (ViewerStateT m)) UserError) a
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
    )

doError :: ( GuiCommand t m ) => SolutionError UserError -> t (SolutionResult UserError m) a
doError = lift . throwE 


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
                    --withGuiComponents $ lift . flip withSolutionTree populateTree
                    lift $ load
                    populateTree
                    lift $ saveSolution $ Just $ projectRoot </> projectName
                InitializerFailed out err -> lift $ throwE $ InvalidOperation (out ++ err) ""

doOpen :: ( GuiCommand t m, MonadIO m )
       => FilePath
       -> t (SolutionResult UserError m)  ()
doOpen path = do
    lift $ openSolution path
    --withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
    populateTree


openItem 
    :: ( GuiCommand t m
       )
    => DeclarationPath
    -> t (SolutionResult UserError m) String
openItem (DeclarationPath pji mi di) = do
    decl <- lift $ getDeclaration pji mi di
    --wrapIOError $ comp `setDeclBufferText` body decl
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
    lift $ prepareBuild
    builder <- lift $ lift $ getBuilder
    lift $ liftIO $ putStrLn "Building"
    r <- lift $ runBuilder builder
    let (text,errors) = case r of
            BuildSucceeded log warnings -> (log,warnings)
            BuildFailed log errors -> (log,errors)
    lift $ liftIO $ putStrLn "Fetching error locations"
    errors' <- lift $ forM errors $ mapErrorM $ \(ErrorLocation proj mod) r c s -> do
        let pji = ProjectInfo $ getProjectName proj
            mi = ModuleInfo $ Symbol $ getModuleName mod
        result <- getModuleItemAtLocation pji mi ((getRow r),(getColumn c))
        case result of
            Just (item, r', c') -> return (ProjectChild pji $ ModuleChild mi $ Just item, Row r', Column c', s)
            Nothing -> return (ProjectChild pji $ ModuleChild mi Nothing, Row 0, Column 0, s)
    lift $ liftIO $ putStrLn "Setting log text"
    splice $ setBuildBufferText text
    lift $ liftIO $ putStrLn "Clearing error list"
    splice $ clearErrorList
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
            {-text <- wrapIOError $ withEditorBuffer comp $ \buffer -> do
                start <- textBufferGetStartIter buffer
                end <- textBufferGetEndIter buffer
                textBufferGetText buffer start end False-}
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
            --withSolutionTree comp populateTree
            populateTree
            when (di /= di') $ do
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

doAddModule :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> t (SolutionResult UserError m)  ()
doAddModule pi mi = do
    lift $ createModule pi mi
    --withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
    populateTree

doRemoveModule :: ( GuiCommand t m )
               => ProjectInfo
               -> ModuleInfo
               -> t (SolutionResult UserError m)  ()
doRemoveModule pi mi = do
    lift $ removeModule pi mi
    --withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
    populateTree


doAddDeclaration :: ( GuiCommand t m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> t (SolutionResult UserError m)  ()
doAddDeclaration pi mi di = do
    let newdecl = WithBody (UnparseableDeclaration di) ""
    lift $ addDeclaration pi mi newdecl
    --withGuiComponents $ \comp -> lift $ do
    --bounce $ withSolutionTree comp populateTree
    populateTree
    decl <- lift $ getDeclaration pi mi di
    --wrapIOError  $ comp `setDeclBufferText` body decl
    splice $ setEditorBufferTextHighlighted $ body decl
    lift $ lift $ setCurrentDecl pi mi di
    splice $ openDeclarationInHistory (DeclarationPath pi mi di) $ body decl

doRemoveDeclaration :: ( GuiCommand t m )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> t (SolutionResult UserError m)  ()
doRemoveDeclaration pi mi di = do
    lift $ removeDeclaration pi mi di
    --withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
    populateTree

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
                    --withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
                    populateTree
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
            _ <- lift $ addImport pi mi (WithBody newImport importStr)
            --withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
            populateTree
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
    --withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
    populateTree


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
            _ <- lift $ addImport pi mi (WithBody newImport importStr)
            --withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
            populateTree
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
            _ <- lift $ addExport pi mi (WithBody newExport exportStr)
            --withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
            populateTree
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
    --withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
    populateTree


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
            _ <- lift $ addExport pi mi (WithBody newExport exportStr)
            --withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
            populateTree
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err

doExportAll :: ( GuiCommand t m )
            => ProjectInfo
            -> ModuleInfo
            -> t (SolutionResult UserError m)  ()
doExportAll pi mi = lift $ exportAll pi mi

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
    {-
    let a = fmap (fmap (fmap sequenceA)) hits :: [ProjectChild [[ModuleChild DeclarationInfo]]]
    let b = fmap (fmap join) a :: [ProjectChild [ModuleChild DeclarationInfo]]
    let c = fmap sequenceA b :: [[ProjectChild (ModuleChild DeclarationInfo)]]
    let d = join c :: [ProjectChild (ModuleChild DeclarationInfo)]
    -}
    let hits' = join . fmap (sequenceA . fmap (join .  fmap sequenceA)) $ hits :: [ProjectChild (ModuleChild DeclarationInfo)]
    case hits' of
        [] -> return ()
        [(ProjectChild pji (ModuleChild mi di))] -> do
            result <- splice $ searchTree $ DeclarationPath pji mi di
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

-- [P [M D]]
-- f (g (f (h a))) -> f (f (g (h a))
