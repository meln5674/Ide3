{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Users where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text ( Text )

import Control.Monad.Reader

import Database.Persist

import Yesod

import Routes

import CredentialsMonad
import Persist
import App
import Auth
import Types

addUser :: SignUpData -> ReaderT Backend Handler (Either AddUserError UserId)
addUser SignUpData{signupUsername, signupPassword, signupEmail} = do
    (gen'dSalt, hashedPassword) <- lift $ hashNewPassword signupPassword
    matchingUsers <- selectList [UserUsername ==. signupUsername] [LimitTo 1]
    case matchingUsers of
        [] -> do
            newId <- insert $ User signupUsername signupEmail hashedPassword gen'dSalt
            return $ Right newId
        _ -> return $ Left UserAlreadyExists
            
getUserById :: UserId -> ReaderT Backend Handler (Maybe User)
getUserById userId = do
    matchingUsers <- selectList [UserId ==. userId] [LimitTo 1]
    case matchingUsers of
        [Entity{ entityVal }] -> return $ Just entityVal
        _ -> return Nothing

getUserByUsername :: (MonadIO m) => Username -> ReaderT Backend m (Maybe (UserId, User))
getUserByUsername username = do
    matchingUsers <- selectList [UserUsername ==. username] [LimitTo 1]
    case matchingUsers of
        [Entity{ entityKey, entityVal }] -> return $ Just (entityKey, entityVal)
        _ -> return Nothing

validateSignIn :: (MonadIO m)
               => SignInData 
               -> ReaderT Backend m (Either SignInError UserId)
validateSignIn signInData@SignInData{signInUsername, signInPassword} = do
    matchingUser <- getUserByUsername signInUsername
    case (signInUsername, signInPassword) of
        (Username "", _) -> return $ Left NoUsername
        (_, "") -> return $ Left NoPassword
        _ -> case matchingUser of
            Just (entityKey, entityVal) -> validateCredentials signInData entityKey entityVal
            _ -> return $ Left AuthorizationError
                
instance CredentialsMonad Handler where
    getCredentials = do
        request <- getRequest
        let params = M.fromList $ reqGetParams request
        case (M.lookup usernameParam params, M.lookup passwordParam params) of
            (Just signInUsername', Just signInPassword) -> do
                let signInUsername = Username signInUsername'
                return $ Just $ SignInData{signInUsername,signInPassword}
            _ -> return Nothing

validateSignInData :: ( MonadIO m
                      , MonadHandler m
                      , CredentialsMonad m
                      )
                   => ReaderT Backend m UserId
validateSignInData = do
    maybeSignInData <- lift $ getCredentials
    case maybeSignInData of
        Just signInData -> do
            result <- validateSignIn signInData
            case result of
                Right userId -> return userId
                Left AuthorizationError -> permissionDenied "Authorization failed"
                Left NoPassword -> invalidArgs [passwordParam]
                Left NoUsername -> invalidArgs [usernameParam]
        Nothing -> invalidArgs [usernameParam, passwordParam]

getUserSolutions :: UserId -> ReaderT Backend Handler [Entity Solution]
getUserSolutions userId = selectList [SolutionOwner ==. userId] []

{-
addUserSolution :: UserId -> SolutionInfo -> ReaderT Backend m (Either AddSolutionError ())
addUserSolution userId solutionInfo = 
-}
