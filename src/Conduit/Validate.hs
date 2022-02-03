{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Conduit.Validate where

import Conduit.Model (SignInForm (..), SignUpForm (..))
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Control.Monad.Trans.Except
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Web.Forma

type SignInFormFields = '["email", "password"]

type SignUpFormFields = '["email", "password", "username"]

signInForm :: Monad m => FormParser SignInFormFields (Text -> Text) m SignInForm
signInForm =
  SignInForm
    <$> field #email (notEmpty >=> validEmail)
    <*> field #password (notEmpty >=> tooShort)

signUpForm :: Monad m => FormParser SignUpFormFields (Text -> Text) m SignUpForm
signUpForm =
  SignUpForm
    <$> field #email (notEmpty >=> validEmail)
    <*> field #password (notEmpty >=> tooShort)
    <*> field #username notEmpty

notEmpty :: Monad m => Text -> ExceptT (Text -> Text) m Text
notEmpty txt =
  if T.null txt
    then throwE (<> " can't be blank")
    else pure txt

tooShort :: Monad m => Text -> ExceptT (Text -> Text) m Text
tooShort txt =
  if T.length txt < 6
    then throwE (<> " must be 6 characters or more")
    else pure txt

validEmail :: Monad m => Text -> ExceptT (Text -> Text) m Text
validEmail txt =
  if isNothing $ T.find (== '@') txt
    then throwE (<> " must contain @ symbol to be valid")
    else pure txt
