{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module SolutionEditor.Stack where

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Distribution.PackageDescription
import Distribution.Version
import Distribution.ModuleName
import Distribution.Text hiding (Text)

import Ide3.Types

import SolutionEditor

import StackMonad

import SolutionEditor.Stack.Types

-- | An Editor that uses the stack new command to create a new solution
stackSolutionEditor :: ( MonadIO m
                       , StackMonad m
                       )
                   => SolutionEditor StackYaml m
stackSolutionEditor = SolutionEditor $ \text -> do
    setStackConfig $ unStackYaml text
    return $ SolutionEditorSucceeded "" ""
