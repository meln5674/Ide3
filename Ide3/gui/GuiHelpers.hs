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
    , makeScrolledWindowWith
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

import Data.GI.Base.Signals hiding (on, after)
import Data.GI.Base.Attributes
import GI.Gtk hiding (on, after, TreePath)

import qualified GI.Gtk as Gtk
import GI.Gdk hiding (Window, on, after)

import GuiClass.Types
{-
class ContainerShell self where
    addToContainer :: (WidgetClass widget) => self -> widget -> IO ()

instance ContainerShell 
-}

{-
makeMenuWith :: ( MenuShellClass self
                , System.Glib.UTFString.GlibString string
                , MonadIO m
                )
             => string -> (Menu -> m b) -> self -> m b
-}
makeMenuWith label f menuBar = do
    menuItem <- liftIO $ menuItemNewWithLabel label
    subMenu <- liftIO $ menuNew
    liftIO $ do
        menuShellAppend menuBar menuItem
        menuItemSetSubmenu menuItem $ Just subMenu
    f subMenu    

--makeWindowWith :: (MonadIO m) => (Window -> m b) -> m b
makeWindowWith f = do
    window <- new Window [ #modal := True]
    --set window [windowModal := True]
    r <- f window
    widgetShowAll window
    return r

--makeOverlayWith :: (MonadIO m, ContainerClass self) => self -> (Overlay -> m b) -> m b
makeOverlayWith container f = do
    overlay <- liftIO $ overlayNew
    liftIO $ container `containerAdd` overlay
    f overlay

{-
makeVBoxWith :: (MonadIO m, ContainerClass self) 
             => self 
             -> (VBox -> m b) 
             -> m b
-}
makeVBoxWith container f = do
    vbox <- liftIO $ vBoxNew False 0
    liftIO $ container `containerAdd` vbox
    f vbox

{-
makeHBoxWith :: (MonadIO m, ContainerClass self) 
             => self 
             -> (HBox -> m b) 
             -> m b
-}
makeHBoxWith window f = do
    hbox <- liftIO $ hBoxNew False 0
    liftIO $ window `containerAdd` hbox
    f hbox

{-
makeNotebookWith :: (MonadIO m, ContainerClass self)
                => self
                -> (Notebook -> m b)
                -> m b
-}
makeNotebookWith self f = do
    notebook <- liftIO $ notebookNew
    liftIO $ self `containerAdd` notebook
    f notebook

{-
makeNotebookPageWith :: (MonadIO m)
                     => Notebook
                     -> String
                     -> (forall self . ContainerClass self => self -> m b)
                     -> m b
-}
makeNotebookPageWith notebook name f = do
    vbox <- liftIO $ vBoxNew False 0
    notebookAppendPage notebook vbox noLabel
    notebookSetTabLabelText notebook vbox name
    f vbox


makeScrolledWindowWith :: (MonadIO m, IsContainer self)
                       => self
                       -> (ScrolledWindow -> m b)
                       -> m b
makeScrolledWindowWith container f = do
    scrollWindow <- scrolledWindowNew noAdjustment noAdjustment
    containerAdd container scrollWindow
    f scrollWindow


{-
makeMenu label menuBar = makeProjectMenuWith menuBar $ \projectMenu -> do
    buildButton <- makeBuildButton projectMenu
    runButton <- makeRunButton projectMenu
    return $ ProjectMenu
             buildButton
             runButton
-}

{-makeMenuButton :: ( MenuShellClass self
                  , System.Glib.UTFString.GlibString string
                  , MonadIO m
                  ) 
               => string -> self -> m MenuItem-}
makeMenuButton label menu = liftIO $ do
    button <- menuItemNewWithLabel label
    menuShellAppend menu button
    return button


{-makeButton :: ( ContainerClass self
              , System.Glib.UTFString.GlibString string
              , MonadIO m
              )
           => string -> self -> m Button-}
makeButton label container = liftIO $ do
    button <- buttonNewWithLabel label
    container `containerAdd` button
    return button

{-makeLabel :: ( ContainerClass self
              , System.Glib.UTFString.GlibString string
              , MonadIO m
              )
           => string -> self -> m Label-}
makeLabel text container = liftIO $ do
    label <- labelNew $ Just text
    container `containerAdd` label
    return label


{-
data GuiSignal2 gui object handler handlerInner where
    GuiSignal2 :: SignalProxy object handlerInner
              -> (gui -> object) 
              -> (HaskellCallbackType handler -> HaskellCallbackType handlerInner)
              -> GuiSignal2 gui object handler handlerInner

type GuiSignal gui object handler = GuiSignal2 gui object handler handler

onGui :: ( SignalInfo handler
         , MonadIO m
         , GObject object
         )
      => gui 
      -> GuiSignal gui object handler 
      -> HaskellCallbackType handler 
      -> m SignalHandlerId
--onGui gui (GuiSignal2 (Signal f) getter modifier)
--    = getter gui `on` (Signal $ \b object handler -> f b object (modifier handler))
onGui gui (GuiSignal2 sig getter modifier) handler
    = getter gui `on` sig $ modifier handler

afterGui :: ( SignalInfo handler
            , MonadIO m 
            , GObject object
            )
         => gui 
         -> GuiSignal gui object handler 
         -> HaskellCallbackType handler 
         -> m SignalHandlerId
--afterGui gui (GuiSignal2 (Signal f) getter modifier)
--    = getter gui `after` (Signal $ \b object handler -> f b object (modifier handler))
afterGui gui (GuiSignal2 sig getter modifier) handler
    = getter gui `after` sig $ modifier handler

{-
onGuiM :: ( SignalInfo info
          , SignalInfoTrans t
          )
       => gui 
       -> t m (GuiSignal2 gui object (t m'' a) (m'' a)) 
       -> HaskellCallbackType (t m'' a)
       -> t m SignalHandlerId
{-onGuiM gui sigM callback = do
    (GuiSignal2 sig getter modifier) <- sigM
    let obj = getter gui
    --let sig' = modifier sig :: _
    let sig' = (undefined :: _) sig :: SignalProxy object (t m'' a)
    liftIO $ on obj sig $ callback
-}
onGuiM = undefined
-}

{-
onGuiF :: (MonadIO (t m), MonadTrans t, MonadIO m, Monad m'', Functor f)
       => gui
       -> t m (GuiSignal2 gui object (f (t m'' a)) (f (m'' a)))
       -> f (t m'' a)
       -> t m (SignalHandlerId object)
onGuiF gui sigM callback = do
    (GuiSignal2 (Signal f) getter modifier) <- sigM
    let obj = getter gui
    let sig' = Signal $ \b object handler -> f b object (modifier handler)
    liftIO $ obj `on` sig' $ callback


afterGuiM :: (MonadIO (t m), MonadTrans t, MonadIO m, Monad m'') 
       => gui 
       -> t m (GuiSignal2 gui object (t m'' a) (m'' a)) 
       -> t m'' a
       -> t m (SignalHandlerId object)
afterGuiM gui sigM callback = do
    (GuiSignal2 (Signal f) getter modifier) <- sigM
    let obj = getter gui
    let sig' = Signal $ \b object handler -> f b object (modifier handler)
    liftIO $ obj `after` sig' $ callback


afterGuiF :: (MonadIO (t m), MonadTrans t, MonadIO m, Monad m'', Functor f)
       => gui
       -> t m (GuiSignal2 gui object (f (t m'' a)) (f (m'' a)))
       -> f (t m'' a)
       -> t m (SignalHandlerId object)
afterGuiF gui sigM callback = do
    (GuiSignal2 (Signal f) getter modifier) <- sigM
    let obj = getter gui
    let sig' = Signal $ \b object handler -> f b object (modifier handler)
    liftIO $ obj `after` sig' $ callback


{-
mkGuiSignal :: (gui -> object) -> Signal object handler -> GuiSignal gui object handler
x `mkGuiSignal` y = GuiSignal (y,x)
-}
-}

mkGuiSignal :: (gui -> object) -> SignalProxy object handler -> GuiSignal gui object handler
x `mkGuiSignal` y = GuiSignal2 y x id



mkGuiSignalWith :: (gui -> object) 
                -> SignalProxy object handlerInner 
                -> (HaskellCallbackType handler -> HaskellCallbackType handlerInner)
                -> GuiSignal2 gui object handler handlerInner
mkGuiSignalWith x y z = GuiSignal2 y x z

{-
editSignal :: Signal object handler -> (handler2 -> handler) -> Signal object handler2
editSignal (Signal f) t = Signal $ \b object handler -> f b object (t handler)
-}

wrapGuiSignal :: (gui -> object) 
              -> GuiSignal2 object subobject handler innerHandler 
              -> GuiSignal2 gui subobject handler innerHandler
wrapGuiSignal f (GuiSignal2 sig getter handler) = GuiSignal2 sig (getter . f) handler
-}


addAccel :: (IsWidget subObject, MonadIO m, Integral key, IsAccelGroup group) 
         => (object -> subObject)
         -> Text
         -> object
         -> group
         -> key
         -> [ModifierType] 
         -> [AccelFlags] 
         -> m ()
addAccel getter signalName object group key modifiers flags = do
    widgetAddAccelerator (getter object) signalName group (fromIntegral key) modifiers flags




{-

x :: object -> SignalInfo GuiEnvT m' T

y :: object -> GuiEnvT m' T -> GuiEnvT m' () ->     m Id

-}


type SubSignalProxy object subObject info = object -> (subObject, SignalProxy subObject info)

ownSignal :: SignalProxy object info -> SubSignalProxy object object info
ownSignal info object = (object, info)

onSub :: (GObject subObject, MonadIO m, SignalInfo info)
      => object
      -> SubSignalProxy object subObject info
      -> HaskellCallbackType info
      -> m SignalHandlerId
onSub object signal callback = let (subObject, signal') = signal object
    in Gtk.on subObject signal' callback

afterSub :: (GObject subObject, MonadIO m, SignalInfo info)
      => object
      -> SubSignalProxy object subObject info
      -> HaskellCallbackType info
      -> m SignalHandlerId
afterSub object signal callback = let (subObject, signal') = signal object
    in Gtk.after subObject signal' callback



class (MonadTrans t) => SignalInterceptClass t where
    intercept :: (Monad m', MonadIO m)
        => ((a -> m' b) -> m c)
        -> (a -> t m' b)
        -> t m c

instance SignalInterceptClass (ReaderT r) where
    intercept add handler = do
        r <- ask
        lift $ add $ \x -> runReaderT (handler x) r

instance SignalInterceptClass IdentityT where
    intercept add handler = lift $ add $ \x -> runIdentityT $ handler x
        

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

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
after2 obj event handler = (afterSub obj event . curry) `intercept` uncurry handler

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

type SubAttrLabelProxy object subObject (attr :: Symbol) = object -> (subObject, AttrLabelProxy attr)

type SubAttrOp object subObject tag = object -> (subObject,AttrOp subObject tag)

getSub :: (AttrGetC info subObj attr result, MonadIO m) => obj -> SubAttrLabelProxy obj subObj attr -> m result
getSub obj subAttr = uncurry get $ subAttr obj

setSub :: MonadIO m => object -> [SubAttrOp object subObject AttrSet] -> m ()
setSub obj subAttrs = mapM_ (uncurry set) x
  where
    x = map (fmap (:[]) . ($obj)) subAttrs 

withTreePath :: MonadIO m => TreePath -> (Gtk.TreePath -> m a) -> m a
withTreePath path f = do
    --liftIO $ putStrLn "Allocating path"
    idiotPath <- mkIdiotPath path
    result <- f idiotPath
    --freeIdiotPath path idiotPath
    --liftIO $ print path
    --liftIO $ putStrLn "Freeing path"
    return result
  where
    mkIdiotPath [] = treePathNew
    mkIdiotPath path = treePathNewFromIndices $ map fromIntegral path
    --freeIdiotPath [] _ = return ()
    --freeIdiotPath _ path = treePathFree path
    
withGtkTreePath :: MonadIO m => Gtk.TreePath -> (TreePath -> m a) -> m a
withGtkTreePath idiotPath f = do
    --liftIO $ putStrLn "AAAAAAAAA"
    path <- liftM (map fromIntegral) $ treePathGetIndices idiotPath
    --liftIO $ putStrLn "BBBBBBBBB"
    --liftIO $ print path
    f path
    
