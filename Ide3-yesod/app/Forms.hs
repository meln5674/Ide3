{-# LANGUAGE OverloadedStrings  #-}
module Forms where

import qualified Data.Text as T

import Yesod

import Ide3.Types

import Types
import App
import Routes

signupForm :: Html -> MForm Handler (FormResult SignUpData, Widget)
signupForm = renderDivs $ SignUpData
    <$> (Username <$> areq textField "User name" Nothing)
    <*> areq textField "Email" Nothing
    <*> areq passwordField "Password" Nothing
    
signInForm :: Html -> MForm Handler (FormResult SignInData, Widget)
signInForm = renderDivs $ SignInData
    <$> (Username <$> areq textField "User name" Nothing)
    <*> areq passwordField "Password" Nothing

newSolutionForm :: Html -> MForm Handler (FormResult NewSolutionData, Widget)
newSolutionForm = renderDivs $ NewSolutionData
    <$> ((SolutionInfo . T.unpack) <$> areq textField "Solution Name" Nothing)
