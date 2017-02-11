{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Auth where

import Data.Text ( Text )
import qualified Data.Text as T

import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as BS

import Data.Map ( Map )
import qualified Data.Map as M

import Control.Monad.Reader

import Crypto.PasswordStore

import Yesod

import Routes
import Persist
import Types

ide3PasswordStrength :: Int
ide3PasswordStrength = 17

usernameParam :: Text
usernameParam = "user"

passwordParam :: Text
passwordParam = "pass"


validateCredentials :: (Monad m)
                    => SignInData 
                    -> UserId 
                    -> User 
                    -> ReaderT Backend m (Either SignInError UserId)
validateCredentials SignInData{signInUsername, signInPassword}
                    entityKey
                    User{userUsername, userPasswordHash, userPasswordSalt} = do
    let signInPasswordBs = BS.pack $ T.unpack signInPassword
    let attemptHash = makePasswordSalt signInPasswordBs userPasswordSalt ide3PasswordStrength
    if attemptHash == userPasswordHash
        then return $ Right entityKey
        else return $ Left AuthorizationError


hashNewPassword :: Text -> Handler (Salt, ByteString)
hashNewPassword signupPassword = do
    gen'dSalt <- liftIO genSaltIO
    let passwordBs = BS.pack $ T.unpack $ signupPassword
    let hashedPassword = makePasswordSalt passwordBs gen'dSalt ide3PasswordStrength
    return (gen'dSalt, hashedPassword)
