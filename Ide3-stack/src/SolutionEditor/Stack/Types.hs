{-# LANGUAGE OverloadedStrings #-}
module SolutionEditor.Stack.Types where

import Data.Text (Text)
import qualified Data.Text as T

import Args

newtype StackYaml = StackYaml { unStackYaml :: Text }

instance Args StackYaml where
    getArgsFrom xs = pure $ StackYaml $ T.intercalate " " $ map T.pack xs
