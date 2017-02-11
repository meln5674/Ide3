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
module Routes where

import Data.Text ( Text )

import Control.Monad

import Yesod

import Ide3.Types

import Types
import Persist

instance PathPiece SolutionInfo where
    toPathPiece (SolutionInfo name) = toPathPiece name
    fromPathPiece = fromPathPiece >=> liftM SolutionInfo

mkYesodData "Ide3Yesod" [parseRoutes|
/ HomeR GET
/users UsersR POST
/users/new UsersNewR GET
!/users/#Username UserProfileByUsernameR GET
/users/#Username/solutions UserSolutionsR GET POST
/users/#Username/solutions/new UserNewSolutionR GET
!/users/#Username/solutions/#SolutionInfo UserSolutionR GET
|]
 
