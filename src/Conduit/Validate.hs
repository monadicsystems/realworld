{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Conduit.Validate where

import Conduit.Model (LoginForm (..), RegisterForm (..), PublishForm (..), EditArticleForm (..))
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Control.Monad.Trans.Except
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Web.Forma

type LoginFormFields = '["email", "password"]

loginForm :: Monad m => FormParser LoginFormFields (Text -> Text) m LoginForm
loginForm =
  LoginForm
    <$> field #email (notEmpty {- >=> validEmail -})
    <*> field #password (notEmpty {- >=> tooShort -})

type RegisterFormFields = '["email", "password", "username"]

registerForm :: Monad m => FormParser RegisterFormFields (Text -> Text) m RegisterForm
registerForm =
  RegisterForm
    <$> field #email (notEmpty {- >=> validEmail -})
    <*> field #password (notEmpty {- >=> tooShort -})
    <*> field #username notEmpty

type PublishFormFields = '["body", "description", "tags", "title"]

publishForm :: Monad m => FormParser PublishFormFields (Text -> Text) m PublishForm
publishForm =
  PublishForm
    <$> field #body notEmpty
    <*> field #description notEmpty
    <*> field #tags notEmpty
    <*> field #title notEmpty

type EditArticleFormFields = '["articleID", "body", "description", "tags", "title"]

editArticleForm :: Monad m => FormParser EditArticleFormFields (Text -> Text) m EditArticleForm
editArticleForm =
  EditArticleForm
    <$> field #articleID (\articleID -> pure articleID)
    <*> field #body notEmpty
    <*> field #description notEmpty
    <*> field #tags notEmpty
    <*> field #title notEmpty

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
