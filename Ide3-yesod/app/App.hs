{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ViewPatterns               #-}
module App where

import Yesod

import Database.Persist
import Database.Persist.Sqlite

import Routes

import Types

instance Yesod Ide3Yesod where
    yesodMiddleware = (sslOnlyMiddleware 120) . defaultYesodMiddleware
    
instance RenderMessage Ide3Yesod FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist Ide3Yesod where
    type YesodPersistBackend Ide3Yesod = SqlBackend
    
    runDB action = do
        Ide3Yesod pool <- getYesod
        runSqlPool action pool


