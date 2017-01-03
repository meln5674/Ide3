{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Dialogs.MoveDeclarationDialog 
    ( MoveDeclarationDialog
    , make
    , confirmClickedEvent
    , cancelClickedEvent
    , getSelectedModulePath
    , close
    ) where

import Data.Maybe
import Data.Tree
import Data.Text (Text, pack)

import Control.Monad.Trans

import Data.GI.Base.Attributes
import GI.Gtk hiding (TreePath, SearchBar)
import GI.Gdk hiding (Window, windowNew)
import Data.GI.Gtk.ModelView.CellLayout
import Data.GI.Gtk.ModelView.ForestStore

import Ide3.Types

import GuiClass
import GuiHelpers

renderSolutionTreeElem :: (AttrSetC info o "text" Text, IsCellRendererText o) 
                       => SolutionTreeElem -> [AttrOp o AttrSet]
renderSolutionTreeElem (ProjectElem (ProjectInfo n)) = [#text := pack n]
renderSolutionTreeElem (ModuleElem (ModuleInfo (Symbol s)) _) = [#text := pack s]
renderSolutionTreeElem (ModuleElem (UnamedModule (Just path)) _) = [#text := pack path]
renderSolutionTreeElem (ModuleElem (UnamedModule Nothing) _) = [#text := ("???" :: Text)]
renderSolutionTreeElem _ = [#text := ("THIS SHOULDN'T BE HERE" :: Text)]

data MoveDeclarationDialog
    = MoveDeclarationDialog
    { window :: Window
    , moduleTree :: TreeView
    , confirmButton :: Button
    , cancelButton :: Button
    }

make :: ( MonadIO m
        , SolutionViewClass m 
        )
     => ( MoveDeclarationDialog -> m a )
     -> m a
make f = makeMoveDeclarationDialogWith $ \window -> do
    renderer <- makeRenderer
    makeVBoxWith window $ \vbox -> do
        moduleTree <- makeModuleTreeWith renderer renderSolutionTreeElem vbox
        makeHBoxWith vbox $ \hbox -> do
            confirmButton <- makeConfirmButtonWith hbox
            cancelButton <- makeCancelButtonWith hbox
            f MoveDeclarationDialog
              { window
              , moduleTree
              , confirmButton
              , cancelButton
              }

makeMoveDeclarationDialogWith :: ( MonadIO m )
                              => ( Window -> m a )
                              -> m a
makeMoveDeclarationDialogWith f = makeWindowWith $ \window -> do
    f window

filterForest :: (a -> Bool) -> Forest a -> Forest a
filterForest f xs = catMaybes $ map (filterTree f) xs

filterTree :: (a -> Bool) -> Tree a -> Maybe (Tree a)
filterTree f (Node x xs)
    | f x = Just $ Node x $ filterForest f xs
    | otherwise = Nothing


makeModuleTreeWith :: ( MonadIO m
                      , SolutionViewClass m
                      , IsContainer self
                      , IsCellRenderer cell
                      )
                   => cell
                   -> ( SolutionTreeElem -> [AttrOp cell AttrSet])
                   -> self
                   -> m TreeView
makeModuleTreeWith renderer renderFunc container
    = makeScrolledWindowWith container $ \scrollWindow -> do
        entireForest <- getForestAtSolutionTreePath []
        let onlyModules = flip filterForest entireForest $ \case
                ProjectElem _ -> True
                ModuleElem _ _ -> True
                _ -> False
        treeViewColumn <- treeViewColumnNew
        treeViewModel <- forestStoreNew onlyModules
        moduleTree <- treeViewNewWithModel treeViewModel
        _ <- treeViewAppendColumn moduleTree treeViewColumn
        cellLayoutPackStart treeViewColumn renderer False
        cellLayoutSetAttributes treeViewColumn renderer treeViewModel renderFunc
        scrollWindow `containerAdd` moduleTree
        return moduleTree

makeRenderer :: (MonadIO m) => m CellRendererText
makeRenderer = cellRendererTextNew

makeConfirmButtonWith :: (MonadIO m, IsContainer self) => self -> m Button
makeConfirmButtonWith = makeButton "Confirm"

makeCancelButtonWith :: (MonadIO m, IsContainer self) => self -> m Button
makeCancelButtonWith = makeButton "Cancel"

type MoveDeclarationDialogSignal object info
    = SubSignalProxy MoveDeclarationDialog object info

mkSignal :: ( MoveDeclarationDialog -> object ) 
         -> SignalProxy object info
         -> MoveDeclarationDialogSignal object info
mkSignal getter signal window = (getter window, signal)

confirmClickedEvent :: MoveDeclarationDialogSignal Button WidgetButtonPressEventSignalInfo
confirmClickedEvent = confirmButton `mkSignal` #buttonPressEvent

cancelClickedEvent :: MoveDeclarationDialogSignal Button WidgetButtonPressEventSignalInfo
cancelClickedEvent = cancelButton `mkSignal` #buttonPressEvent

getSelectedModulePath :: (MonadIO m)
                      => MoveDeclarationDialog
                      -> m (Maybe (TreePath, TreeViewColumn))
getSelectedModulePath window = do
    result <- treeViewGetCursor (moduleTree window)
    case result of
        (Just path'', Just column) -> do
            path' <- treePathGetIndices path''
            let path = map fromIntegral path'
            --liftIO $ print path
            --treePathFree path''
            return $ Just (path, column)
        _ -> return Nothing

close :: ( MonadIO m )
      => MoveDeclarationDialog
      -> m ()
close = widgetDestroy . window
