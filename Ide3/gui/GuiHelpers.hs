{-# LANGUAGE GADTs #-}
module GuiHelpers 
    ( makeMenuWith
    , makeMenuButton
    , makeWindowWith
    , GuiSignal
    , GuiSignal2
    , onGui
    , onGuiM
    , onGuiF
    , mkGuiSignal
    , mkGuiSignalWith
    , editSignal
    ) where

import Control.Monad.Trans

import System.Glib.UTFString

import Graphics.UI.Gtk

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
    liftIO $ windowSetModal window True
    r <- f window
    liftIO $ widgetShowAll window
    return r

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
