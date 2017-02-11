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
module Handlers where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text ( Text )

import Control.Monad.Except
import Control.Monad.Reader

import Ide3.NewMonad (PersistenceClass)
import qualified Ide3.NewMonad as Ide3
import Ide3.Types

import Ide3.NewMonad.Instances.UserT

import Yesod
import Routes
import Persist
import Types
import Auth
import Users
import Forms
import CredentialsMonad
import UserT

mkYesodDispatch "Ide3Yesod" resourcesIde3Yesod

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
    <p>Hello Word!
    <a href=@{UsersNewR}>Sign Up
|]

{-
getUserProfileByIdR :: UserId -> Handler Html
getUserProfileByIdR otherUserId = do
    userId <- runDB validateSignInData
    if otherUserId == userId 
        then defaultLayout [whamlet|Undefined|]
        else permissionDenied "Permission Denied"
-}

verifyUsernameMatches :: ( MonadHandler m
                         , CredentialsMonad m
                         )
                      => Username 
                      -> ( UserId -> User -> ReaderT Backend m a )
                      -> ReaderT Backend m a
                      -> ReaderT Backend m a
verifyUsernameMatches otherUsername onMatch onFail = do
    userId <- validateSignInData
    maybeOtherUser <- getUserByUsername otherUsername
    case maybeOtherUser of
        Just (userId, user@User{userUsername})
            | otherUsername == userUsername -> onMatch userId user
        _ -> onFail

getUserProfileByUsernameR :: Username -> Handler Html
getUserProfileByUsernameR urlUsername = runDB $ 
    verifyUsernameMatches urlUsername onMatch onFail
  where
    onMatch _ _ = lift $ defaultLayout [whamlet|Undefined|]
    onFail = lift $ permissionDenied "Permisson deined"

getUsersNewR :: Handler Html
getUsersNewR = do
    (widget, enctype) <- generateFormPost signupForm
    defaultLayout
        [whamlet|
            <p>
                Sign up
            <form method=post action=@{UsersR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]


postUsersR :: Handler Html
postUsersR = do
    ((result, widget), enctype) <- runFormPost signupForm
    case result of
        FormSuccess signupData -> do
            addUserResult <- runDB $ addUser signupData
            case addUserResult of
                Right _ -> defaultLayout [whamlet|<p>Sign up Successful|]
                Left UserAlreadyExists -> defaultLayout 
                    [whamlet|
                        <p>That Username is Taken
                        <form method=post action=@{UsersNewR} enctype=#{enctype}>
                            ^{widget}
                        <button>Submit
                    |]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input
                <form method=post action=@{UsersNewR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

{-
getSignInR = do
    (widget, enctype) <- generateFormPost signInForm
    defaultLayout
        [whamlet|
            <p>
                Log In
            <form method=post action=@{SignInR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]

postApiSessionR = do
    ((result, widget), enctype) <- runFormPost signInForm
    case result of
        FormSuccess signInData -> do
            validateUserResult <- runDB $ validateSignIn signInData
            case validateUserResult of
                Right userid -> do
                    defaultLayout [whamlet|SignIn Successful|]
                Left err -> do
                    defaultLayout [whamlet|SignIn Failed: #{show err}|]
-}

getUserSolutionsR :: Username -> Handler Html
getUserSolutionsR urlUsername = runDB $ 
    verifyUsernameMatches urlUsername onMatch onFail
  where
    onMatch userId _ = do
        userSolutions <- getUserSolutions userId
        lift $ defaultLayout
            [whamlet|
                <p>Solutions
                <ul>
                    $forall Entity solutionId (Solution solutionInfo@(SolutionInfo solutionName) _) <- userSolutions
                        <li>
                            <a href=@{UserSolutionR urlUsername solutionInfo}>#{solutionName}
            |]
    onFail = lift $ permissionDenied "Permission Denied"

doWithSolution :: UserId
               -> SolutionInfo 
               -> (SolutionId -> ReaderT Backend Handler a)
               -> ReaderT Backend Handler a
               -> ReaderT Backend Handler a
doWithSolution userId solutionInfo onMatch onFail = do
    matches <- selectList [SolutionName ==. solutionInfo, SolutionOwner ==. userId] [LimitTo 1]
    case matches of
        [Entity{entityKey}] -> onMatch entityKey
        _ -> onFail
    
getUserSolutionR :: Username -> SolutionInfo -> Handler Html
getUserSolutionR urlUsername urlSolutionInfo = runDB $ 
    verifyUsernameMatches urlUsername onMatch onFail
  where
    onMatch _ _ = lift $ defaultLayout [whamlet|Undefined|]
    onFail = lift $ permissionDenied "Permission Denied"

postUserSolutionsR :: Username -> Handler Html
postUserSolutionsR urlUsername = runDB $
    verifyUsernameMatches urlUsername onMatch onFail
  where
    onMatch userId user = do
        ((result, widget), enctype) <- lift $ runFormPost newSolutionForm
        case result of
            FormSuccess NewSolutionData{newSolutionInfo} -> do
                newResult <- flip runUserT (Entity userId user) $ runExceptT $ do
                    Ide3.new newSolutionInfo
                    Ide3.finalize undefined
                case newResult of
                    Left err -> do
                        let _ = err :: SolutionError SolutionUserError
                        lift $ defaultLayout
                            [whamlet|
                                <p>
                                    New Solution
                                    #{show err}
                                <form method=post action=@{UserSolutionsR urlUsername} enctype=#{enctype}>
                                    ^{widget}
                                    <button>Submit
                            |]
                    Right _ -> lift $ redirect (UserSolutionR urlUsername newSolutionInfo)
    onFail = permissionDenied "Permission Denied"

getUserNewSolutionR :: Username -> Handler Html
getUserNewSolutionR urlUsername = runDB $
    verifyUsernameMatches urlUsername onMatch onFail
  where
    onMatch _ _ = lift $ do
        (widget, enctype) <- generateFormPost newSolutionForm
        defaultLayout
            [whamlet|
                <p>
                    New Solution
                <form method=post action=@{UserSolutionsR urlUsername} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]
    onFail = lift $ permissionDenied "permissonDenied"
