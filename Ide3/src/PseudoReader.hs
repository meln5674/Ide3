{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-|
Module      : PseudoReader
Description : Typeclass for things which behave like ReaderT
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

Types in the PseudoReader class expose a function which takes a value of that
type, some kind of Reader, and returns a value and an updated Reader within any monad
-}
module PseudoReader where

import Control.Monad.Trans.Reader

-- | Typeclass for transformers which behave similarly to ReaderT
class PseudoReaderT t r | t -> r where
    -- | Equivalent of runReaderT
    runPseudoReaderT :: (Monad m) => t m a -> r -> m a


instance PseudoReaderT (ReaderT r) r where
    runPseudoReaderT = runReaderT

