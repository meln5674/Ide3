{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Dialogs.MainWindow.Components 
    ( makeSolutionViewer
    , makeBuildViewer
    , renderSolutionTreeElem
    , module Dialogs.MainWindow.Components.Types
    , module Dialogs.MainWindow.Components.Signals
    ) where

import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad
import Control.Monad.Trans

import Data.GI.Base.Attributes
import GI.Gtk
import Data.GI.Gtk.ModelView.CellLayout

import Ide3.Types

import ErrorParser.Types

import DeclarationPath
import GuiClass.Types
import GuiEnv
import GuiMonad

import GuiHelpers

import BetterTextView

import Dialogs.MainWindow.Components.Types
import Dialogs.MainWindow.Components.Signals

makeSolutionViewer :: ( MonadIO m
                      , IsContainer self
                      , IsCellRenderer cell
                      ) 
                   => cell
                   -> (SolutionTreeElem -> [AttrOp cell 'AttrSet])
                   -> self
                   -> GuiEnvT {-proxy-} m' p  m SolutionViewer
makeSolutionViewer renderer renderFunc container = do
    makeHPanedWith container $ \hbox -> do
        projectViewBox <- makeSoloBox
        declViewBox <- makeSoloBox
        hbox `panedAdd1` projectViewBox
        hbox `panedAdd2` declViewBox
        projectView <- makeProjView renderer projectViewBox renderFunc
        declView <- makeDeclView declViewBox
        declBuffer <- withGuiComponents $ flip withEditorBuffer return
        return SolutionViewer
               { projectView
               , declView
               , declBuffer
               }

makeProjView :: ( MonadIO m
                , IsContainer self
                , IsCellRenderer cell
                ) 
             => cell 
             -> self 
             -> (SolutionTreeElem -> [AttrOp cell 'AttrSet]) 
             -> GuiEnvT {-proxy-} m' p  m TreeView
makeProjView renderer container renderFunc = makeScrolledWindowWith container $ \scrollWindow -> do
    treeViewColumn <- treeViewColumnNew
    projView <- withGuiComponents $ flip withSolutionTree treeViewNewWithModel
    _ <- treeViewAppendColumn projView treeViewColumn
    cellLayoutPackStart treeViewColumn renderer False
    withGuiComponents $ flip withSolutionTree $ \treeStore -> 
        cellLayoutSetAttributes treeViewColumn renderer treeStore renderFunc
    scrollWindow `containerAdd` projView
    --projView `treeViewSetTooltipColumn` index
    set projView [#hasTooltip := True]
    return projView

makeDeclView :: ( MonadIO m
                , IsContainer self
                ) 
             => self -> GuiEnvT {-proxy-} m' p  m BetterTextView
makeDeclView container = makeScrolledWindowWith container $ \scrollWindow -> do
    declView <- withGuiComponents $ flip withEditorBuffer $ betterTextViewNewWithBuffer
    setSub declView $ map mkBTVAttr [ #monospace := True
                                    , #editable := False
                                    ]
    scrollWindow `containerAdd` declView
    return declView

makeBuildViewer :: ( MonadIO m
                   , IsContainer self
                   ) 
                => self -> GuiEnvT {-proxy-} m' p  m BuildViewer
makeBuildViewer container = do
    makeNotebookWith container $ \notebook -> do
        buildView <- makeNotebookPageWith notebook "Log" makeBuildView
        set buildView [#editable := False]
        errorView <- makeNotebookPageWith notebook "Errors" makeErrorView
        return BuildViewer
             { buildView
             , errorView
             }

makeBuildView :: ( MonadIO m
                 , IsContainer self
                 ) 
              => self -> GuiEnvT {-proxy-} m' p  m TextView
makeBuildView container = makeScrolledWindowWith container $ \scrollWindow -> do
    buildView <- withGuiComponents $ flip withBuildBuffer textViewNewWithBuffer
    set buildView [ #editable := False ]
    scrollWindow `containerAdd` buildView
    return buildView


makeErrorView :: ( MonadIO m
                 , IsContainer self
                 )
              => self -> GuiEnvT {-proxy-} m' p m TreeView
makeErrorView container = do
    withGuiComponents 
        $ flip withErrorList 
        $ \list -> makeScrolledWindowWith container 
        $ \scrollWindow -> liftIO $ do

            errorView <- treeViewNewWithModel list
            
            imageColumn <- treeViewColumnNew
            projectColumn <- treeViewColumnNew
            moduleColumn <- treeViewColumnNew
            declarationColumn <- treeViewColumnNew
            rowColumn <- treeViewColumnNew
            columnColumn <- treeViewColumnNew
            messageColumn <- treeViewColumnNew

            renderer <- cellRendererTextNew
            imageRenderer <- cellRendererPixbufNew
            
            treeViewColumnSetTitle projectColumn "Project"
            treeViewColumnSetTitle moduleColumn "Module"
            treeViewColumnSetTitle declarationColumn "Location"
            treeViewColumnSetTitle rowColumn "Row"
            treeViewColumnSetTitle columnColumn "Column"
            treeViewColumnSetTitle messageColumn "Message"
            
            treeViewColumnPackStart imageColumn imageRenderer True
            treeViewColumnPackStart projectColumn renderer True
            treeViewColumnPackStart moduleColumn renderer True
            treeViewColumnPackStart declarationColumn renderer True
            treeViewColumnPackStart rowColumn renderer True
            treeViewColumnPackStart columnColumn renderer True
            treeViewColumnPackStart messageColumn renderer True
            
            cellLayoutSetAttributes imageColumn imageRenderer list renderImageCell
            cellLayoutSetAttributes projectColumn renderer list renderProjectCell
            cellLayoutSetAttributes moduleColumn renderer list renderModuleCell
            cellLayoutSetAttributes declarationColumn renderer list renderDeclarationCell
            cellLayoutSetAttributes rowColumn renderer list renderRowCell
            cellLayoutSetAttributes columnColumn renderer list renderColumnCell
            cellLayoutSetAttributes messageColumn renderer list renderMessageCell
            
            {-
            treeViewColumnSetExpand declarationColumn True
            -}
            treeViewColumnSetExpand messageColumn True
            
            
            void $ treeViewAppendColumn errorView imageColumn
            void $ treeViewAppendColumn errorView projectColumn
            void $ treeViewAppendColumn errorView moduleColumn
            void $ treeViewAppendColumn errorView declarationColumn
            void $ treeViewAppendColumn errorView rowColumn
            void $ treeViewAppendColumn errorView columnColumn
            void $ treeViewAppendColumn errorView messageColumn
            
            set projectColumn [treeViewColumnResizable := True]
            set moduleColumn [treeViewColumnResizable := True]
            set declarationColumn [treeViewColumnResizable := True]
            set messageColumn [treeViewColumnResizable := True]
            
            scrollWindow `containerAdd` errorView
            
            return errorView


-- | Renderer for the solution tree
renderSolutionTreeElem :: (AttrSetC info o "text" Text) 
                       => SolutionTreeElem 
                       -> [AttrOp o 'AttrSet]
renderSolutionTreeElem (ProjectElem (ProjectInfo n)) = [#text := n]
renderSolutionTreeElem (ModuleElem (ModuleInfo (Symbol s)) _) = [#text := s]
renderSolutionTreeElem (DeclElem (SymbolDeclarationInfo sym))
    = [#text := getSymbol sym]
renderSolutionTreeElem (DeclElem (RawDeclarationInfo s))
    = [#text := s]
renderSolutionTreeElem ImportsElem = [#text := ("Imports" :: Text)]
renderSolutionTreeElem ExportsElem = [#text := ("Exports" :: Text)]
renderSolutionTreeElem PragmasElem = [#text := ("Pragmas" :: Text)]
renderSolutionTreeElem (ImportElem _ (WithBody _ importBody)) = [#text := importBody] 
renderSolutionTreeElem (ExportElem _ (WithBody _ exportBody)) = [#text := exportBody] 
renderSolutionTreeElem (PragmaElem p) = [#text := p]
renderSolutionTreeElem (UnparsableModuleElem (ModuleInfo (Symbol s)) _ _)
    = [#text := (s <> " (UNPARSEABLE)" :: Text)]
renderSolutionTreeElem SolutionElem = [#text := ("THIS SHOULDN'T BE SEEN" :: Text)]


-- | Renderer for the error list image column
renderImageCell :: (AttrSetC info o "stockId" Text) 
                => Error ItemPath Text
                -> [AttrOp o 'AttrSet]
renderImageCell (Warning _ _ _ _) = [#stockId := STOCK_DIALOG_WARNING]
renderImageCell (Error _ _ _ _) = [#stockId := STOCK_DIALOG_ERROR]

-- | Renderer for the error list project column
renderProjectCell :: (AttrSetC info o "text" Text)
                  => Error ItemPath Text
                  -> [AttrOp o 'AttrSet]
renderProjectCell e = [#text := unProjectInfo pji]
  where
    (ProjectChild pji _) = errorLocation e

-- | Renderer for the error list module column
renderModuleCell :: (AttrSetC info o "text" Text) 
                 => Error ItemPath Text
                 -> [AttrOp o 'AttrSet]
renderModuleCell e = [#text := moduleInfoString mi]
  where
    (ProjectChild _ (ModuleChild mi _)) = errorLocation e

-- | Renderer for the error list declaration column
renderDeclarationCell :: (AttrSetC info o "text" Text)
              => Error ItemPath Text
              -> [AttrOp o 'AttrSet]
renderDeclarationCell err = [#text := text]
  where
    (ProjectChild _ (ModuleChild _ x)) = errorLocation err
    text = case x of
        Just (HeaderCommentString _) -> "[MODULE HEADER]"
        Just (PragmaString p) -> "[PRAGMA] " <> p
        Just (ImportString i) -> body i
        Just (ExportString e) -> "[EXPORT] " <> body e
        Just (DeclarationString di) -> case item di of
            SymbolDeclarationInfo sym -> getSymbol sym
            RawDeclarationInfo text -> text
        Nothing -> ""

-- | Renderer for the error list row column
renderRowCell :: (AttrSetC info o "text" Text) 
              => Error ItemPath Text
              -> [AttrOp o 'AttrSet]
renderRowCell (Warning _ row _ _) = [#text := T.pack (show row)]
renderRowCell (Error _ row _ _) = [#text := T.pack (show row)]

-- | Renderer for the error list column column
renderColumnCell :: (AttrSetC info o "text" Text)
                 => Error ItemPath Text
                 -> [AttrOp o 'AttrSet]
renderColumnCell (Warning _ _ col _) = [#text := T.pack (show col)]
renderColumnCell (Error _ _ col _) = [#text := T.pack (show col)]

-- | Renderer for the error list message column
renderMessageCell :: (AttrSetC info o "text" Text) 
                  => Error ItemPath Text
                  -> [AttrOp o 'AttrSet]
renderMessageCell (Warning _ _ _ msg) = [#text := msg]
renderMessageCell (Error _ _ _ msg) = [#text := msg]



