{-# LANGUAGE OverloadedStrings #-}

module Conduit.Link where
import Conduit.Model
import Data.Text
import Conduit.Core

homeLink :: Text
homeLink = "/"

authorFeedLink :: ID User -> Text
authorFeedLink (ID userID) = "/feed/author/" <> textShow userID

favoriteFeedLink :: ID User -> Text
favoriteFeedLink (ID userID) = "/feed/favorite/" <> textShow userID

globalFeedLink :: Text
globalFeedLink = "/feed/global/"

tagFeedLink :: Tag -> Text
tagFeedLink = ("/feed/tag/" <>) . unTag

settingsLink :: Text
settingsLink = "/settings"

logoutLink :: Text
logoutLink = "/logout"

registerFormLink :: Text
registerFormLink = "/register"

registerLink :: Text
registerLink = registerFormLink

loginFormLink :: Text
loginFormLink = "/login"

loginLink :: Text
loginLink = "/login"
