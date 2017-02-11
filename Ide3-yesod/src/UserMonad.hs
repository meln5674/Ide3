module UserMonad.hs where

data UserMonadError
    = UserAlreadyExists
    | UserDoesNotExist
    | InvalidCredentials

type UserMonadResult m = ExceptT UserMonadError m

class Monad m => UserMonad m where
    type UserName m
    type Password m
    addUser :: UserName m -> Password m -> UserMonadResult m ()
    removeUser :: UserName m -> UserMonadResult m ()
    validateUser :: UserName m -> UserMonadResult m ()
