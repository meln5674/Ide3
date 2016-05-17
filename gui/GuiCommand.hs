{-# LANGUAGE ScopedTypeVariables, PolyKinds #-}
module GuiCommand where

import Graphics.UI.Gtk

import Data.Proxy

import Control.Concurrent

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Monad

import Viewer
import ViewerMonad2

import GuiMonad
import ProjectTree

type UserError = ()

dialogOnError :: (ViewerMonad m, InteruptMonad2 p m)
              => MVar (ViewerState,p)
              -> a
              -> ProjectResult (ViewerStateT m) UserError a
              -> IO a
dialogOnError var default_ f = do
    r <- interupt1 var (runExceptT f)
    case r of
        Right x -> do
            liftIO $ putStrLn "Finishing state success"
            return x
        Left e -> do
            liftIO $ putStrLn "Finishing state failure"
            dialog <- messageDialogNew
                Nothing
                []
                MessageError
                ButtonsClose
                (show e)
            dialogRun dialog
            --dialogDestroy dialog
            widgetDestroy dialog
            liftIO $ putStrLn "Closed"
            return default_

doOpen :: forall proxy m buffer p 
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          )
       => proxy m
       -> GuiComponents buffer
       -> FilePath
       -> MVar (ViewerState,p)
       -> IO ()
doOpen _ comp path var = dialogOnError var () $ 
    flip asTypeOf (undefined :: ProjectResult (ViewerStateT m) UserError ()) $ do
        openProject path
        withProjectTree comp $ populateTree
