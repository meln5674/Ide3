module GuiHelpers where

import Graphics.UI.Gtk

makeMenuWith label f menuBar = do
    menuItem <- menuItemNewWithLabel label
    subMenu <- menuNew
    menuShellAppend menuBar menuItem
    menuItemSetSubmenu menuItem subMenu
    f subMenu    

{-
makeMenu label menuBar = makeProjectMenuWith menuBar $ \projectMenu -> do
    buildButton <- makeBuildButton projectMenu
    runButton <- makeRunButton projectMenu
    return $ ProjectMenu
             buildButton
             runButton
-}

makeMenuButton label menu = do
    button <- menuItemNewWithLabel label
    menuShellAppend menu button
    return button

newtype GuiSignal gui object handler
    = GuiSignal
    ( Signal object handler
    , gui -> object 
    )

onGui :: gui -> GuiSignal gui object callback -> callback -> IO (ConnectId object)
onGui gui (GuiSignal (sig,get)) = (get gui) `on` sig

mkGuiSignal :: (gui -> object) -> Signal object handler -> GuiSignal gui object handler
x `mkGuiSignal` y = GuiSignal (y,x)

--mkGuiSignalWith :: (gui -> object) -> Signal object handler handler2 -> (handler -> handler2) -> GuiSignal gui object handler
--mkGuiSignalWith x y z = GuiSignal (y,x,z)
