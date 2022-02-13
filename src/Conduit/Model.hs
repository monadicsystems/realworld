{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Conduit.Model where

import Data.Aeson
import Data.Int (Int32)
import Data.Text
import Data.Time
import GHC.Generics
import Servant.Auth.JWT (FromJWT, ToJWT)
import Web.FormUrlEncoded (FromForm)

newtype ID a = ID {unID :: Int32} deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- CORE MODELS START --

data User = User
  { userBio :: Text,
    userEmail :: Text,
    userID :: ID User,
    userImageUrl :: Text,
    userUsername :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, FromJWT, ToJSON, ToJWT)

data Article = Article
  { articleAuthorID :: ID User,
    articleBody :: Text,
    articleCreatedAt :: UTCTime,
    articleDescription :: Text,
    articleFavorites :: Int32,
    articleID :: ID Article,
    articleTitle :: Text
  }
  deriving (Eq, Show)

newtype Tag = Tag
  { unTag :: Text
  }
  deriving (Eq, Show)

data Comment = Comment
  { commentArticleID :: ID Article,
    commentAuthorID :: ID User,
    commentBody :: Text,
    commentCreatedAt :: UTCTime,
    commentID :: ID Comment
  }
  deriving (Eq, Show)

data Follow = (ID User) :-> (ID User)

-- CORE MODELS END --

-- FORM MODELS START --

data SignInForm = SignInForm
  { signInFormEmail :: Text,
    signInFormPassword :: Text
  }
  deriving (FromForm, Generic, Show)

instance ToJSON SignInForm where
  toJSON (SignInForm email password) =
    object
      [ "email" .= email,
        "password" .= password
      ]

data SignUpForm = SignUpForm
  { signUpFormEmail :: Text,
    signUpFormPassword :: Text,
    signUpFormUsername :: Text
  }
  deriving (FromForm, Generic, Show)

instance ToJSON SignUpForm where
  toJSON (SignUpForm email password username) =
    object
      [ "email" .= email,
        "password" .= password,
        "username" .= username
      ]

data SettingsForm = SettingsForm
  { settingsFormBio :: Text,
    settingsFormEmail :: Text,
    settingsFormImageUrl :: Text,
    settingsFormNewPassword :: Text,
    settingsFormUserID :: ID User,
    settingsFormUsername :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data NewEditorForm = NewEditorForm
  { newEditorFormAuthorID :: ID User,
    newEditorFormBody :: Text,
    newEditorFormDescription :: Text,
    newEditorFormTags :: Text,
    newEditorFormTitle :: Text
  }
  deriving (Eq, Show)

data UpdateEditorForm = UpdateEditorForm
  { updateEditorFormArticleID :: ID Article,
    updateEditorFormBody :: Text,
    updateEditorFormDescription :: Text,
    updateEditorFormTags :: Text,
    updateEditorFormTitle :: Text
  }
  deriving (Eq, Show)

data CommentForm = CommentForm
  { commentFormArticleID :: ID Article,
    commentFormAuthorID :: ID User,
    commentFormBody :: Text
  }
  deriving (Eq, Show)

newtype FollowForm = FollowForm
  { followFormTarget :: Text
  }
  deriving (FromForm, Generic, Show)

newtype UnfollowForm = UnfollowForm
  { unfollowFormTarget :: Text
  }
  deriving (FromForm, Generic, Show)

-- FORM MODELS END --
