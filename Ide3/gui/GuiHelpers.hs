{-# LANGUAGE GADTs #-}
module GuiHelpers 
    ( makeMenuWith
    , makeMenuButton
    , makeButton
    , makeLabel
    , makeWindowWith
    , makeOverlayWith
    , makeVBoxWith
    , makeHBoxWith
    , GuiSignal
    , GuiSignal2
    , onGui
    , onGuiM
    , onGuiF
    , afterGui
    , afterGuiM
    , afterGuiF
    , mkGuiSignal
    , mkGuiSignalWith
    , editSignal
    , wrapGuiSignal
    , addAccel
    ) where

import Data.Text

import Control.Monad.Trans

import System.Glib.UTFString

import Graphics.UI.Gtk

{-
class ContainerShell self where
    addToContainer :: (WidgetClass widget) => self -> widget -> IO ()

instance ContainerShell 
-}

makeMenuWith :: ( MenuShellClass self
                , System.Glib.UTFString.GlibString string
                , MonadIO m
                )
             => string -> (Menu -> m b) -> self -> m b
makeMenuWith label f menuBar = do
    menuItem <- liftIO $ menuItemNewWithLabel label
    subMenu <- liftIO $ menuNew
    liftIO $ do
        menuShellAppend menuBar menuItem
        menuItemSetSubmenu menuItem subMenu
    f subMenu    

makeWindowWith :: (MonadIO m) => (Window -> m b) -> m b
makeWindowWith f = do
    window <- liftIO $ windowNew
    liftIO $ set window [windowModal := True]
    r <- f window
    liftIO $ widgetShowAll window
    return r

makeOverlayWith :: (MonadIO m, ContainerClass self) => self -> (Overlay -> m b) -> m b
makeOverlayWith container f = do
    overlay <- liftIO $ overlayNew
    liftIO $ container `containerAdd` overlay
    f overlay

makeVBoxWith :: (MonadIO m, ContainerClass self) 
             => self 
             -> (VBox -> m b) 
             -> m b
makeVBoxWith container f = do
    vbox <- liftIO $ vBoxNew False 0
    liftIO $ container `containerAdd` vbox
    f vbox

makeHBoxWith :: (MonadIO m, ContainerClass self) 
             => self 
             -> (HBox -> m b) 
             -> m b
makeHBoxWith window f = do
    hbox <- liftIO $ hBoxNew False 0
    liftIO $ window `containerAdd` hbox
    f hbox



{-
makeMenu label menuBar = makeProjectMenuWith menuBar $ \projectMenu -> do
    buildButton <- makeBuildButton projectMenu
    runButton <- makeRunButton projectMenu
    return $ ProjectMenu
             buildButton
             runButton
-}

makeMenuButton :: ( MenuShellClass self
                  , System.Glib.UTFString.GlibString string
                  , MonadIO m
                  ) 
               => string -> self -> m MenuItem
makeMenuButton label menu = liftIO $ do
    button <- menuItemNewWithLabel label
    menuShellAppend menu button
    return button


makeButton :: ( ContainerClass self
              , System.Glib.UTFString.GlibString string
              , MonadIO m
              )
           => string -> self -> m Button
makeButton label container = liftIO $ do
    button <- buttonNewWithLabel label
    container `containerAdd` button
    return button

makeLabel :: ( ContainerClass self
              , System.Glib.UTFString.GlibString string
              , MonadIO m
              )
           => string -> self -> m Label
makeLabel text container = liftIO $ do
    label <- labelNew $ Just text
    container `containerAdd` label
    return label



data GuiSignal2 gui object handler handlerInner where
    GuiSignal2 :: Signal object handlerInner 
              -> (gui -> object) 
              -> (handler -> handlerInner)
              -> GuiSignal2 gui object handler handlerInner

type GuiSignal gui object handler = GuiSignal2 gui object handler handler

--newtype GuiSignal gui object handler = GuiSignal (Signal object handler, gui -> object)


onGui :: gui -> GuiSignal gui object handler -> handler -> IO (ConnectId object)
onGui gui (GuiSignal2 (Signal f) getter modifier)
    = getter gui `on` (Signal $ \b object handler -> f b object (modifier handler))

afterGui :: gui -> GuiSignal gui object handler -> handler -> IO (ConnectId object)
afterGui gui (GuiSignal2 (Signal f) getter modifier)
    = getter gui `after` (Signal $ \b object handler -> f b object (modifier handler))

{-
onGuiEnv :: (MonadIO m, Monad m'') => gui 
         -> GuiEnvT proxy m' p buffer m (GuiSignal gui object (GuiEnvT proxy m' p buffer m'' a) (m'' a)) 
         -> GuiEnvT proxy m' p buffer m'' a
         -> GuiEnvT proxy m' p buffer m (ConnectId object)
-}
onGuiM :: (MonadIO (t m), MonadTrans t, MonadIO m, Monad m'') 
       => gui 
       -> t m (GuiSignal2 gui object (t m'' a) (m'' a)) 
       -> t m'' a
       -> t m (ConnectId object)
onGuiM gui sigM callback = do
    (GuiSignal2 (Signal f) getter modifier) <- sigM
    let obj = getter gui
    let sig' = Signal $ \b object handler -> f b object (modifier handler)
    liftIO $ obj `on` sig' $ callback


onGuiF :: (MonadIO (t m), MonadTrans t, MonadIO m, Monad m'', Functor f)
       => gui
       -> t m (GuiSignal2 gui object (f (t m'' a)) (f (m'' a)))
       -> f (t m'' a)
       -> t m (ConnectId object)
onGuiF gui sigM callback = do
    (GuiSignal2 (Signal f) getter modifier) <- sigM
    let obj = getter gui
    let sig' = Signal $ \b object handler -> f b object (modifier handler)
    liftIO $ obj `on` sig' $ callback


afterGuiM :: (MonadIO (t m), MonadTrans t, MonadIO m, Monad m'') 
       => gui 
       -> t m (GuiSignal2 gui object (t m'' a) (m'' a)) 
       -> t m'' a
       -> t m (ConnectId object)
afterGuiM gui sigM callback = do
    (GuiSignal2 (Signal f) getter modifier) <- sigM
    let obj = getter gui
    let sig' = Signal $ \b object handler -> f b object (modifier handler)
    liftIO $ obj `after` sig' $ callback


afterGuiF :: (MonadIO (t m), MonadTrans t, MonadIO m, Monad m'', Functor f)
       => gui
       -> t m (GuiSignal2 gui object (f (t m'' a)) (f (m'' a)))
       -> f (t m'' a)
       -> t m (ConnectId object)
afterGuiF gui sigM callback = do
    (GuiSignal2 (Signal f) getter modifier) <- sigM
    let obj = getter gui
    let sig' = Signal $ \b object handler -> f b object (modifier handler)
    liftIO $ obj `after` sig' $ callback


{-
mkGuiSignal :: (gui -> object) -> Signal object handler -> GuiSignal gui object handler
x `mkGuiSignal` y = GuiSignal (y,x)
-}


mkGuiSignal :: (gui -> object) -> Signal object handler -> GuiSignal gui object handler
x `mkGuiSignal` y = GuiSignal2 y x id



mkGuiSignalWith :: (gui -> object) 
                -> Signal object handlerInner 
                -> (handler -> handlerInner) 
                -> GuiSignal2 gui object handler handlerInner
mkGuiSignalWith x y z = GuiSignal2 y x z

editSignal :: Signal object handler -> (handler2 -> handler) -> Signal object handler2
editSignal (Signal f) t = Signal $ \b object handler -> f b object (t handler)

wrapGuiSignal :: (gui -> object) 
              -> GuiSignal2 object subobject handler innerHandler 
              -> GuiSignal2 gui subobject handler innerHandler
wrapGuiSignal f (GuiSignal2 sig getter handler) = GuiSignal2 sig (getter . f) handler

{-addAccel :: (WidgetClass object, MonadIO m) 
         => (MainWindow -> object) 
         -> String 
         -> MainWindow 
         -> _ 
         -> String 
         -> [Modifier] 
         -> [AccelFlags] 
         -> m ()-}
addAccel f e w g kn ms fs = do
    k <- keyvalFromName $ pack kn
    widgetAddAccelerator (f w) e g k ms fs
