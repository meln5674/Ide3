{-|
Module      : Ide3.NewMonad.Instances.State.Class
Description : Classes for the stateful implementation of the NewMonad
                   typeclasses
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Ide3.NewMonad.Instances.State.Class 
    ( module Ide3.NewMonad.Instances.State.Class.Internal
    ) where

import Ide3.NewMonad.Instances.Lift.TH

import Ide3.Utils

import Ide3.NewMonad.Instances.State.Class.Internal

[betterderiving| splice2; ; StatefulPersistenceClass; StatefulWrapper; |]

