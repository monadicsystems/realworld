{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Conduit.Model where

import Data.Aeson
import Data.Text
import Data.Time
import GHC.Generics
import Servant.Auth.JWT (FromJWT, ToJWT)

newtype ID a = ID {unID :: Int} deriving (Eq, Show)

data WithMeta a = WithMeta
  { withMetaCreatedAt :: Maybe UTCTime,
    withMetaData :: a,
    withMetaID :: ID a,
    withMetaUpdatedAt :: Maybe UTCTime
  }
  deriving (Eq, Show)

data User = User
  { userBio :: Text,
    userEmail :: Text,
    userImage :: Text,
    userUsername :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, FromJWT, ToJSON, ToJWT)

data Article = Article
  { articleBody :: Text,
    articleDescription :: Text,
    articleFavoritesCount :: Int
  }
  deriving (Eq, Show)

newtype Tag = Tag
  { unTag :: Text
  }
  deriving (Eq, Show)

newtype Comment = Comment
  { unComment :: Text
  }
  deriving (Eq, Show)

data SignInForm = SignInForm deriving (FromJSON, Generic)

data SignUpForm = SignUpForm deriving (FromJSON, Generic)

{-
{
  "article": {
    "slug": "how-to-train-your-dragon",
    "title": "How to train your dragon",
    "description": "Ever wonder how?",
    "body": "It takes a Jacobian",
    "tagList": ["dragons", "training"],
    "createdAt": "2016-02-18T03:22:56.637Z",
    "updatedAt": "2016-02-18T03:48:35.824Z",
    "favorited": false,
    "favoritesCount": 0,
    "author": {
      "username": "jake",
      "bio": "I work at statefarm",
      "image": "https://i.stack.imgur.com/xHWG8.jpg",
      "following": false
    }
  }
}
-}
