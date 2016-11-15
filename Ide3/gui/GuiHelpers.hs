{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
module GuiHelpers 
    ( makeMenuWith
    , makeMenuButton
    , makeButton
    , makeLabel
    , makeWindowWith
    , makeOverlayWith
    , makeVBoxWith
    , makeHBoxWith
    , makeNotebookWith
    , makeNotebookPageWith
    , makeStackWith
    , makeStackChildWith
    , makeScrolledWindowWith
    , makeLabelEntryPairWith
    , makeLabelComboBoxPairWith
    , listStoreAppend
    , withTreePath
    , withGtkTreePath
    , SignalInterceptClass (..)
    , onSub
    , afterSub
    , SubSignalProxy
    , ownSignal
    , on
    , on1
    , on2
    , on3
    , after
    , after1
    , after2
    , after3
    , SubAttrLabelProxy
    , SubAttrOp
    , getSub
    , setSub
    , addAccel
    ) where

import GHC.TypeLits

import Data.Text hiding (map)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity

import Data.GI.Base.GValue
import Data.GI.Base.Signals hiding (on, after)
import Data.GI.Base.Attributes
import GI.Gtk hiding (on, after, TreePath, listStoreAppend)
import Data.GI.Gtk.ModelView.SeqStore

import qualified GI.Gtk as Gtk
import GI.Gdk hiding (Window, on, after)

import GuiClass.Types



makeLabelEntryPairWith :: ( MonadIO m
                          , IsContainer self
                          ) 
                       => Text 
                       -> self
                       -> ( Label -> Entry -> EntryBuffer -> m b )
                       -> m b
makeLabelEntryPairWith txt self f = do
    lbl <- makeLabel txt self
    (entry, buf) <- makeEntry self
    f lbl entry buf

makeLabelComboBoxPairWith :: ( MonadIO m
                             , IsContainer self
                             ) 
                          => Text 
                          -> self
                          -> ( Label -> ComboBox -> SeqStore a -> m b )
                          -> m b
makeLabelComboBoxPairWith txt self f = do
    lbl <- makeLabel txt self
    (box, model) <- makeComboBox self
    f lbl box model

-- | Make a GTK window and apply an action to it
makeWindowWith :: (MonadIO m) => (Window -> m b) -> m b
makeWindowWith f = do
    window <- new Window [ #modal := True]
    r <- f window
    widgetShowAll window
    return r


-- | Create a GTK menu, add it to a container, then apply an action to it
makeMenuWith :: ( IsMenuShell self
                , MonadIO m
                )
             => Text -> (Menu -> m b) -> self -> m b
makeMenuWith label f menuBar = do
    menuItem <- liftIO $ menuItemNewWithLabel label
    subMenu <- liftIO $ menuNew
    liftIO $ do
        menuShellAppend menuBar menuItem
        menuItemSetSubmenu menuItem $ Just subMenu
    f subMenu    


-- | Add a GTK overlay to a container, then apply an action to it
makeOverlayWith :: ( MonadIO m
                   , IsContainer self
                   ) 
                => self -> (Overlay -> m b) -> m b
makeOverlayWith container f = do
    overlay <- liftIO $ overlayNew
    liftIO $ container `containerAdd` overlay
    f overlay

-- | Add a GTK VBox to a container, then apply an action to it
makeVBoxWith :: ( MonadIO m
                , IsContainer self
                )
             => self 
             -> (VBox -> m b) 
             -> m b
makeVBoxWith container f = do
    vbox <- liftIO $ vBoxNew False 0
    liftIO $ container `containerAdd` vbox
    f vbox

-- | Add a GTK HBox to a container, then apply an action to it
makeHBoxWith :: ( MonadIO m
                , IsContainer self
                )
             => self 
             -> (HBox -> m b) 
             -> m b
makeHBoxWith window f = do
    hbox <- liftIO $ hBoxNew False 0
    liftIO $ window `containerAdd` hbox
    f hbox

-- | Add a GTK notebook to a container, then apply an action to it
makeNotebookWith :: ( MonadIO m
                    , IsContainer self
                    )
                => self
                -> (Notebook -> m b)
                -> m b
makeNotebookWith self f = do
    notebook <- liftIO $ notebookNew
    liftIO $ self `containerAdd` notebook
    f notebook

-- | Add a page to a GTK nodebook, then apply an action to it
makeNotebookPageWith :: ( MonadIO m )
                     => Notebook
                     -> Text
                     -> (forall self . IsContainer self => self -> m b)
                     -> m b
makeNotebookPageWith notebook name f = do
    vbox <- liftIO $ vBoxNew False 0
    notebookAppendPage notebook vbox noLabel
    notebookSetTabLabelText notebook vbox name
    f vbox

makeStackWith :: ( MonadIO m
                 , IsContainer self
                 )
              => self
              -> ( Stack -> m b)
              -> m b
makeStackWith self f = do
    stack <- stackNew
    self `containerAdd` stack
    f stack

makeStackChildWith :: ( MonadIO m
                      )
                   => Stack
                   -> Text
                   -> ( forall self . IsContainer self => self -> m b)
                   -> m b
makeStackChildWith self name f = do
    vbox <- liftIO $ vBoxNew False 0
    stackAddNamed self vbox name
    f vbox

-- | Add a GTK scrolled window to a container, then apply an action to it
makeScrolledWindowWith :: ( MonadIO m
                          , IsContainer self
                          )
                       => self
                       -> (ScrolledWindow -> m b)
                       -> m b
makeScrolledWindowWith container f = do
    scrollWindow <- scrolledWindowNew noAdjustment noAdjustment
    containerAdd container scrollWindow
    f scrollWindow

-- | Add a GTK menu to a container, then apply an action to it
makeMenuButton :: ( IsMenuShell self
                  , MonadIO m
                  ) 
               => Text
               -> self 
               -> m MenuItem
makeMenuButton label menu = liftIO $ do
    button <- menuItemNewWithLabel label
    menuShellAppend menu button
    return button

-- | Add a GTK button to a container with the given text
makeButton :: ( IsContainer self
              , MonadIO m
              )
           => Text -> self -> m Button
makeButton label container = liftIO $ do
    button <- buttonNewWithLabel label
    container `containerAdd` button
    return button

-- | Add a GTK label to a container with the given text
makeLabel :: ( IsContainer self
             , MonadIO m
             )
           => Text 
           -> self
           -> m Label
makeLabel text container = liftIO $ do
    label <- labelNew $ Just text
    container `containerAdd` label
    return label

makeEntry :: ( IsContainer self
             , MonadIO m
             )
          => self
          -> m (Entry, EntryBuffer)
makeEntry self = liftIO $ do
    buf <- entryBufferNew Nothing 0
    entry <- entryNewWithBuffer buf
    self `containerAdd` entry
    return (entry, buf)

makeComboBox :: ( IsContainer self
                , MonadIO m
                )
             => self
             -> m (ComboBox, SeqStore a)
makeComboBox self = liftIO $ do
    model <- seqStoreNew []
    box <- comboBoxNewWithModel model
    self `containerAdd` box
    return (box, model)

listStoreAppend :: ( MonadIO m, IsGValue a )
                => ListStore
                -> a
                -> m ()
listStoreAppend model value = do
    iter <- Gtk.listStoreAppend model
    gvalue <- liftIO $ toGValue value
    listStoreSetValue model iter 0 gvalue

-- | Add an accelerator (keyboard shortcut) to a widget
addAccel :: ( IsWidget subObject
            , MonadIO m
            , Integral key
            , IsAccelGroup group
            )
         => (object -> subObject)
         -> Text
         -> object
         -> group
         -> key
         -> [ModifierType] 
         -> [AccelFlags] 
         -> m ()
addAccel getter signalName object group key modifiers flags = do
    widgetAddAccelerator (getter object) 
                         signalName 
                         group 
                         (fromIntegral key) 
                         modifiers 
                         flags


-- | Signals which affect widgets inside of a data structure
type SubSignalProxy object subObject info 
    =  object 
    -> (subObject, SignalProxy subObject info) -- ^ 
    -- Such signals are represented by functions mapping the data structure to a
    -- pair of widget and signals on that widget

-- | Create a SubSignalProxy in which the data structure is the widget itself
ownSignal :: SignalProxy object info -> SubSignalProxy object object info
ownSignal info object = (object, info)

-- | Attach a handler to a widget inside of a data structure
onSub :: (GObject subObject, MonadIO m, SignalInfo info)
      => object
      -> SubSignalProxy object subObject info
      -> HaskellCallbackType info
      -> m SignalHandlerId
onSub object signal callback = let (subObject, signal') = signal object
    in Gtk.on subObject signal' callback

-- | Attach a handler to a widget inside of a data structure, the handler fires
-- after the default handler
afterSub :: (GObject subObject, MonadIO m, SignalInfo info)
      => object
      -> SubSignalProxy object subObject info
      -> HaskellCallbackType info
      -> m SignalHandlerId
afterSub object signal callback = let (subObject, signal') = signal object
    in Gtk.after subObject signal' callback

-- | Class of transformers which can 'intercept' the attaching of signals to
-- objects
class (MonadTrans t) => SignalInterceptClass t where
    intercept :: ( Monad m'
                 , MonadIO m
                 )
              => ((a -> m' b) -> m c) -- ^ Function which would add a signal
              -> (a -> t m' b)        -- ^ The handler inside of the transformer
              -> t m c

-- | Intercept signals by providing the environment to the handler
instance SignalInterceptClass (ReaderT r) where
    intercept add handler = do
        r <- ask
        lift $ add $ \x -> runReaderT (handler x) r

-- | Intercept signals by unwrapping them
instance SignalInterceptClass IdentityT where
    intercept add handler = lift $ add $ \x -> runIdentityT $ handler x
        
-- | 3-arity version of curry
curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

-- | 3-arity version of uncurry
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

-- | Intercepted signal attaching function for handlers which are 0-arity
on :: ( GObject subObject
      , Monad m'
      , MonadIO m
      , SignalInterceptClass t
      , SignalInfo info
      , HaskellCallbackType info ~ m' b
      )
   => object
   -> SubSignalProxy object subObject info
   -> t m' b
   -> t m SignalHandlerId
on obj event handler =
    (onSub obj event . ($())) `intercept` const handler

-- | Intercepted signal attaching function for handlers which are 1-arity
on1 :: ( GObject subObject
       , Monad m'
       , MonadIO m
       , SignalInterceptClass t
       , SignalInfo info
       , HaskellCallbackType info ~ (a -> m' b)
       )
    => object
    -> SubSignalProxy object subObject info
    -> (a -> t m' b)
    -> t m SignalHandlerId
on1 obj event handler = do
    onSub obj event `intercept` handler

-- | Intercepted signal attaching function for handlers which are 2-arity
on2 :: ( GObject subObject
       , Monad m'
       , MonadIO m
       , SignalInterceptClass t
       , SignalInfo info
       , HaskellCallbackType info ~ (a -> b -> m' c)
       )
    => object
    -> SubSignalProxy object subObject info
    -> (a -> b -> t m' c)
    -> t m SignalHandlerId
on2 obj event handler = (onSub obj event . curry) `intercept` uncurry handler

-- | Intercepted signal attaching function for handlers which are 3-arity
on3 :: ( GObject subObject
       , Monad m'
       , MonadIO m
       , SignalInterceptClass t
       , SignalInfo info
       , HaskellCallbackType info ~ (a -> b -> c -> m' d)
       )
    => object
    -> SubSignalProxy object subObject info
    -> (a -> b -> c -> t m' d)
    -> t m SignalHandlerId
on3 obj event handler = (onSub obj event . curry3) `intercept` uncurry3 handler

-- | Intercepted signal attaching function for handlers which are 0-arity,
-- handler runs after the default handler
after :: ( GObject subObject
         , Monad m'
         , MonadIO m
         , SignalInterceptClass t
         , SignalInfo info
         , HaskellCallbackType info ~ m' b
         )
      => object
      -> SubSignalProxy object subObject info
      -> t m' b
      -> t m SignalHandlerId
after obj event handler =
    (afterSub obj event . ($())) `intercept` const handler

-- | Intercepted signal attaching function for handlers which are 1-arity,
-- handler runs after the default handler
after1 :: ( GObject subObject
          , Monad m'
          , MonadIO m
          , SignalInterceptClass t
          , SignalInfo info
          , HaskellCallbackType info ~ (a -> m' b)
          )
       => object
       -> SubSignalProxy object subObject info
       -> (a -> t m' b)
       -> t m SignalHandlerId
after1 obj event handler = do
    afterSub obj event `intercept` handler

-- | Intercepted signal attaching function for handlers which are 2-arity,
-- handler runs after the default handler
after2 :: ( GObject subObject
          , Monad m'
          , MonadIO m
          , SignalInterceptClass t
          , SignalInfo info
          , HaskellCallbackType info ~ (a -> b -> m' c)
          )
       => object
       -> SubSignalProxy object subObject info
       -> (a -> b -> t m' c)
       -> t m SignalHandlerId
after2 obj event handler =
    (afterSub obj event . curry) `intercept` uncurry handler

-- | Intercepted signal attaching function for handlers which are 3-arity,
-- handler runs after the default handler
after3 :: ( GObject subObject
          , Monad m'
          , MonadIO m
          , SignalInterceptClass t
          , SignalInfo info
          , HaskellCallbackType info ~ (a -> b -> c -> m' d)
          )
       => object
       -> SubSignalProxy object subObject info
       -> (a -> b -> c -> t m' d)
       -> t m SignalHandlerId
after3 obj event handler = (afterSub obj event . curry3) `intercept` uncurry3 handler

-- | Attributes for widgets inside of data structures
type SubAttrLabelProxy object subObject (attr :: Symbol)
    = object -> (subObject, AttrLabelProxy attr)

-- | Attribute operatios for widgets inside of data structures
type SubAttrOp object subObject tag = object -> (subObject,AttrOp subObject tag)

-- | Retreive the value of an attribute of a widget in a data structure
getSub :: ( AttrGetC info subObj attr result
          , MonadIO m
          )
       => obj 
       -> SubAttrLabelProxy obj subObj attr 
       -> m result
getSub obj subAttr = uncurry get $ subAttr obj

-- | Assign the value of an attribute of a widget in a data structure
setSub :: MonadIO m => object -> [SubAttrOp object subObject AttrSet] -> m ()
setSub obj subAttrs = mapM_ (uncurry set) x
  where
    x = map (fmap (:[]) . ($obj)) subAttrs 

-- | Perform an action after allocating a tree path
withTreePath :: ( MonadIO m ) => TreePath -> (Gtk.TreePath -> m a) -> m a
withTreePath path f = do
    path' <- allocPath path
    result <- f path'
    {-result `seq` treePathFree path'-}
    {-result `seq` return result-}
    return result
  where
    allocPath [] = treePathNew
    allocPath path = treePathNewFromIndices $ map fromIntegral path

-- | Perform an action with the underlying tree path from an alocated tree
-- path    
withGtkTreePath :: MonadIO m => Gtk.TreePath -> (TreePath -> m a) -> m a
withGtkTreePath idiotPath f = do
    path <- liftM (map fromIntegral) $ treePathGetIndices idiotPath
    f path
    
