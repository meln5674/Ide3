module CredentialsMonad where

import Types

class (Monad m) => CredentialsMonad m where
    getCredentials :: m (Maybe SignInData)
