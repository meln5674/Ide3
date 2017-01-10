{-|
Module      : GuiHelpers
Description : Convienience wrappers for GTK, dealing with signals and attributes
                for widgets contained in data structures, and signals which
                require context
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

In addition to providng functions for building GUIs using GTK, this module also
provides the functionality for dealing with widgets contained in data
structures. This is accomplished by creating functions which accept a data
structure and return a pair of the appropriate widget and the signal or
attribute on that widget. This, along with the data structure in question, are
provided to a helper function which uses the underlying GTK functions for
attaching signals and using attributes.

Finally, this module provides a way to deal with signal handlers which require
context, such as a static reference to a widget or buffer. GTK signals may only
accept handlers in the IO monad, no context is permitted. This is solved by
a typeclass which provides a function that accepts the signal attaching
function, as well as a handler inside of a transformer. The tranformer is then
used to provide the context, yielding a handler in the underlying monad, which
is usually IO, which can then be attached using the normal signal attach
function.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
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
    , withTreePath
    , withGtkTreePath
    , SignalInterceptClass (..)
    , onSub
    , afterSub
    , SubSignalProxy
    , ownSignal
    , FuncClass (..)
    , Func0 (Func0)
    , Func1 (Func1)
    , Func2 (Func2)
    , Func3 (Func3)
    , Func4 (Func4)
    , Func5 (Func5)
    , Func6 (Func6)
    , on
--    , on_
    , after
--    , after_
    , SubAttrLabelProxy
    , SubAttrOp
    , getSub
    , setSub
    , addAccel
    ) where

import GHC.TypeLits

import Data.Text hiding (map, group)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity

import Data.GI.Base.Signals hiding (on, after)
import Data.GI.Base.Attributes
import GI.Gtk hiding (on, after, TreePath)
import Data.GI.Gtk.ModelView.SeqStore

import qualified GI.Gtk as Gtk
import GI.Gdk hiding (Window, on, after)

import GuiClass.Types


-- | Add a GTK label and entry, along with buffer, to a container, then apply an
-- action to them
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

-- | Add a GTK label and combobox, along with a sequence model, to a container,
-- then apply an action to them
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


-- | Add a GTK menu to a container, then apply an action to it
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
    void $ notebookAppendPage notebook vbox noLabel
    notebookSetTabLabelText notebook vbox name
    f vbox

-- | Add a GTK stack to a container, then apply an action to it
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

-- | Add a page to a GTK stack, then apply an action to it
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

-- | Add a GTK entry to a container along with a buffer
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

-- | Add a GTK combobox to a container along with a sequence model
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


-- | Signals which affect widgets inside of a data structure.
-- Such signals are represented by functions mapping the data structure to a
-- pair of widget and signal on that widget
type SubSignalProxy object subObject info 
    =  object -> (subObject, SignalProxy subObject info)

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
-- objects to provide additional context
class (MonadTrans t) => SignalInterceptClass t where
    -- | Take a handler attaching function which gives no context, a handler
    -- which requires context, then attach the handler after supplying it the
    -- context
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

-- | Intercept signals and provide no context
instance SignalInterceptClass IdentityT where
    intercept add handler = lift $ add $ \x -> runIdentityT $ handler x
        
-- | 3-arity version of curry
curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

-- | 3-arity version of uncurry
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c


newtype Func0 a = Func0 { unFunc0 :: a }
newtype Func1 a b = Func1 { unFunc1 :: a -> b }
newtype Func2 a b c = Func2 { unFunc2 :: a -> b -> c }
newtype Func3 a b c d = Func3 { unFunc3 :: a -> b -> c -> d }
newtype Func4 a b c d e = Func4 { unFunc4 :: a -> b -> c -> d -> e}
newtype Func5 a b c d e f = Func5 { unFunc5 :: a -> b -> c -> d -> e -> f}
newtype Func6 a b c d e f g =  Func6 { unFunc6 :: a -> b -> c -> d -> e -> f -> g}

type family FuncArgType x = arg where
    FuncArgType Func0 = ()
    FuncArgType (Func1 a) = a
    FuncArgType (Func2 a b) = (a, b)

type family FuncType x z = f where
    FuncType Func0 z = z
    FuncType (Func1 a) z = a -> z
    FuncType (Func2 a b) z = a -> b -> z

class FuncClass x where
    uncurry' :: x a -> (FuncArgType x -> a)
    curry' :: (FuncArgType x -> a) -> x a
    mkFunc :: FuncType x a -> x a
    unmkFunc :: x a -> FuncType x a

instance FuncClass Func0 where
    uncurry' = const . unFunc0 
    curry' = Func0 . ($())
    mkFunc = Func0
    unmkFunc = unFunc0


instance FuncClass (Func1 a) where
    uncurry' = unFunc1
    curry' = Func1
    mkFunc = Func1
    unmkFunc = unFunc1

instance FuncClass (Func2 a b) where
    uncurry' = uncurry . unFunc2
    curry' = Func2 . curry
    mkFunc = Func2
    unmkFunc = unFunc2


{-
newtype Func0 m a = Func0 { unFunc0 :: m a }
newtype Func1 a m b = Func1 { unFunc1 :: a -> m b }
newtype Func2 a b m c = Func2 { unFunc2 :: a -> b -> m c }
newtype Func3 a b c m d = Func3 { unFunc3 :: a -> b -> c -> m d }
newtype Func4 a b c d m e = Func4 { unFunc4 :: a -> b -> c -> d -> m e}
newtype Func5 a b c d e m f = Func5 { unFunc5 :: a -> b -> c -> d -> e -> m f}
newtype Func6 a b c d e f m g =  Func6 { unFunc6 :: a -> b -> c -> d -> e -> f -> m g}

type family FuncArgType x = arg where
    FuncArgType Func0 = ()
    FuncArgType (Func1 a) = a
    FuncArgType (Func2 a b) = (a, b)

type family FuncType x m z = f | f -> x m z where
    FuncType Func0 m z = m z
    FuncType (Func1 a) m z = a -> m z
    FuncType (Func2 a b) m z = a -> b -> m z

class FuncClass x where
    uncurry' :: x m a -> (FuncArgType x -> m a)
    curry' :: (FuncArgType x -> m a) -> x m a
    mkFunc :: FuncType x m a -> x m a
    unmkFunc :: x m a -> FuncType x m a

instance FuncClass Func0 where
    uncurry' = const . unFunc0 
    curry' = Func0 . ($())
    mkFunc = Func0
    unmkFunc = unFunc0


instance FuncClass (Func1 a) where
    uncurry' = unFunc1
    curry' = Func1
    mkFunc = Func1
    unmkFunc = unFunc1

instance FuncClass (Func2 a b) where
    uncurry' = uncurry . unFunc2
    curry' = Func2 . curry
    mkFunc = Func2
    unmkFunc = unFunc2
-}

{-
instance FuncClass (Func3 a b c) where
    --type FuncArgType (Func3 a b c) = (a, b, c)
    --type FuncType (Func3 a b c) d = a -> b -> c -> d
    uncurry' (Func3 f) = \(a, b, c) -> f a b c
    curry' f = Func3 $ \a b c -> f (a, b, c)

instance FuncClass (Func4 a b c d) where
    --type FuncArgType (Func4 a b c d) = (a, b, c, d)
    --type FuncType (Func4 a b c d) e = a -> b -> c -> d -> e
    uncurry' (Func4 f) = \(a, b, c, d) -> f a b c d
    curry' f = Func4 $ \a b c d -> f (a, b, c, d)

instance FuncClass (Func5 a b c d e) where
    --type FuncArgType (Func5 a b c d e) = (a, b, c, d, e)
    --type FuncType (Func5 a b c d e) f = a -> b -> c -> d -> e -> f
    uncurry' (Func5 f) = \(a, b, c, d, e) -> f a b c d e
    curry' f = Func5 $ \a b c d e -> f (a, b, c, d, e)

instance FuncClass (Func6 a b c d e f') where
    --type FuncArgType (Func6 a b c d e f') = (a, b, c, d, e, f')
    --type FuncType (Func6 a b c d e f) g = a -> b -> c -> d -> e -> f -> g
    uncurry' (Func6 f) = \(a, b, c, d, e, f') -> f a b c d e f'
    curry' f = Func6 $ \a b c d e f' -> f (a, b, c, d, e, f')
-}

on :: forall object subObject info handler m t m' b
    . ( GObject subObject
      , Monad m'
      , MonadIO m
      , SignalInterceptClass t
      , SignalInfo info
      , HaskellCallbackType info ~ FuncType handler (m' b)
      , FuncClass handler
      )
   => object
   -> SubSignalProxy object subObject info
   -> handler (t m' b)
   -> t m SignalHandlerId
on obj event handler' = intercept add handler
  where
    add :: (FuncArgType handler -> m' b) -> m SignalHandlerId
    add h = onSub obj event $ unmkFunc $ (curry' h :: handler (m' b))
    handler :: FuncArgType handler -> t m' b
    handler = uncurry' $ (handler' :: handler (t m' b))

{-
on_ :: forall object subObject info handler m t m' b
     . ( GObject subObject
       , Monad m'
       , MonadIO m
       , SignalInterceptClass t
       , SignalInfo info
       , HaskellCallbackType info ~ FuncType handler (m' b)
       , FuncClass handler
       , Functor (t m)
       )
    => object
    -> SubSignalProxy object subObject info
    -> FuncType handler (t m' b)
    -> t m ()
on_ obj event handler = void $ (on :: object 
        -> SubSignalProxy object subObject info 
        -> FuncType handler (t m' b) 
        -> t m SignalHandlerId) obj event handler
-}

after :: forall object subObject info handler m t m' b
       . ( GObject subObject
         , Monad m'
         , MonadIO m
         , SignalInterceptClass t
         , SignalInfo info
         , HaskellCallbackType info ~ FuncType handler (m' b)
         , FuncClass handler
         )
      => object
      -> SubSignalProxy object subObject info
      -> handler (t m' b)
      -> t m SignalHandlerId
after obj event handler' = intercept add handler
  where
    add :: (FuncArgType handler -> m' b) -> m SignalHandlerId
    add h = afterSub obj event $ unmkFunc $ (curry' h :: handler (m' b))
    handler :: FuncArgType handler -> t m' b
    handler = uncurry' $ (handler' :: handler (t m' b))

{-
after_ :: ( GObject subObject
          , Monad m'
          , MonadIO m
          , SignalInterceptClass t
          , SignalInfo info
          , HaskellCallbackType info ~ FuncType handler (m' b)
          , FuncClass handler
          , Functor (t m)
          )
       => object
       -> SubSignalProxy object subObject info
       -> FuncType handler (t m' b)
       -> t m ()
after_ obj event handler = void $ after obj event handler
-}

{-
-- | Attach a handler requring context and no arguments
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


on_ obj event handler = void $ on obj event handler

-- | Attach a handler requring context and 1 argument
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

on1_ obj event handler = void $ on1 obj event handler

-- | Attach a handler requring context and 2 arguments
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

on2_ obj event handler = void $ on2 obj event handler

-- | Attach a handler requring context and 3 arguments
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

on3_ obj event handler = void $ on3 obj event handler


-- | Same as 'on', except the handler fires after the default one
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

after_ obj event handler = void $ after obj event handler

-- | Same as 'on1', except the handler fires after the default one
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

after1_ obj event handler = void $ after1 obj event handler

-- | Same as 'on2', except the handler fires after the default one
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

after2_ obj event handler = void $ after2 obj event handler

-- | Same as 'on3', except the handler fires after the default one
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

after3_ obj event handler = void $ after3 obj event handler
-}


-- | Attributes for widgets inside of data structures.
-- These attributes are represented as functions mapping the data structure to
-- a pair of widget and attribute on that widget
type SubAttrLabelProxy object subObject (attr :: Symbol)
    = object -> (subObject, AttrLabelProxy attr)

-- | Attribute operations for widgets inside of data structures.
-- These attributes are represented as functions mapping the data structure to
-- a pair of widget and attribute operation on that widget
type SubAttrOp object subObject tag 
    = object -> (subObject, AttrOp subObject tag)

-- | Retrieve the value of an attribute of a widget in a data structure
getSub :: ( AttrGetC info subObj attr result
          , MonadIO m
          )
       => obj 
       -> SubAttrLabelProxy obj subObj attr 
       -> m result
getSub obj subAttr = uncurry get $ subAttr obj

-- | Assign the value of an attribute of a widget in a data structure
setSub :: MonadIO m => object -> [SubAttrOp object subObject 'AttrSet] -> m ()
setSub obj subAttrs = mapM_ (uncurry set) x
  where
    x = map (fmap (:[]) . ($obj)) subAttrs 

-- | Perform an action after allocating a tree path.
-- NOTE: The path is not manually freed due to gi-gtk doing this automatically
-- when the path is garbage collected. Freeing manually results in a
-- double-free, and there doesn't appear to be a way to stop this.
withTreePath :: ( MonadIO m ) => TreePath -> (Gtk.TreePath -> m a) -> m a
withTreePath purePath f = do
    gtkPath <- allocPath purePath
    result <- f gtkPath
    return result
  where
    allocPath [] = treePathNew
    allocPath path = treePathNewFromIndices $ map fromIntegral path

-- | Perform an action with the underlying tree path from an alocated tree
-- path    
withGtkTreePath :: MonadIO m => Gtk.TreePath -> (TreePath -> m a) -> m a
withGtkTreePath gtkPath f = do
    purePath <- liftM (map fromIntegral) $ treePathGetIndices gtkPath
    f purePath
    
