module Signal where

import Graphics.UI.Gtk

newtype GuiSignal gui object handler = GuiSignal ( Signal object handler, gui -> object )

onGui :: gui -> GuiSignal gui object callback -> callback -> IO (ConnectId object)
onGui gui (GuiSignal (sig,get)) = (get gui) `on` sig

mkGuiSignal :: (gui -> object) -> Signal object handler -> GuiSignal gui object handler
x `mkGuiSignal` y = GuiSignal (y,x)
