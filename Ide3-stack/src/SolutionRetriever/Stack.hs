{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module SolutionRetriever.Stack where

import Data.Text (Text)

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Distribution.PackageDescription
import Distribution.Text hiding (Text)

import Ide3.Types

import SolutionRetriever

import StackMonad

import SolutionEditor.Stack.Types

stackSolutionRetriever :: ( MonadIO m
                          , StackMonad m
                          )
                       => SolutionRetriever StackYaml m
stackSolutionRetriever = SolutionRetriever $ StackYaml <$> getStackConfig
