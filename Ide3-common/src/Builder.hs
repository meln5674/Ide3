{-|
Module      : Builder
Description : Building solutions
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A Builder is an abstract data type which attempts to build a solution
-}

{-# LANGUAGE RankNTypes #-}
module Builder 
    ( Builder (..)
    , BuilderResult (..)
    , runBuilder
    , noBuilder
    , mapBuilder
    )
    where
import Data.Text (Text)

import Control.Monad.Trans.Except

import Ide3.Types

import ErrorParser.Types

{-
-- | The result of a build operation
data BuilderResult
    = BuildFailed Text [Error (ErrorLocation Text) Text] -- ^ Build failed, accompanied by log and errors/warnings
    | BuildSucceeded Text [Error (ErrorLocation Text) Text] -- ^ Build succeeded, accompanied by log and warnings
-}

type OnLine m = Text -> m ()
type OnError m = Error (ErrorLocation Text) Text -> m ()

data BuilderResult = BuildFailed | BuildSucceeded

-- | The builder abstract type. Use runBuilder to execute the actions of a Builder.
-- A build can failed in one of two ways. A ExceptT Left value indicates that
-- the build could not start, did not complete, etc. A BuildFailed value indicates
-- that the build went through but did not compile or link successfully.
newtype Builder m = MkBuilder
    { runBuilderInternal :: forall u . OnLine m -> OnError m -> SolutionResult u m BuilderResult }

-- | Execute the actions of a builder inside a monad.
runBuilder :: Builder m -> OnLine m -> OnError m -> SolutionResult u m BuilderResult
runBuilder = runBuilderInternal

-- | A builder which represents having no build capabilities and will always result in an erro
noBuilder :: Monad m => Builder m
noBuilder = MkBuilder $ \_ _ -> throwE $ Unsupported "No builder specified"

mapBuilder :: (forall a . m a -> m' a) -> (forall a . m' a -> m a) -> Builder m -> Builder m'
mapBuilder f g (MkBuilder b) = MkBuilder $ \l e -> mapExceptT f (b (fmap g l) (fmap g e))

