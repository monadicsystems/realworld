{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Conduit.Resource.Get.Profile where

import Conduit.App
import Conduit.Core
import Conduit.Link
import qualified Conduit.Model as Model
import qualified Conduit.Template as Template
import Control.Monad (forM_)
import Data.Proxy
import Data.Text
import Lucid
import Lucid.Htmx
import Lucid.Htmx.Servant
import Lucid.Hyperscript (useHyperscript, __, _hs)
import Servant
import Servant.Auth
import Servant.Auth.Server
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import Servant.Links
import Servant.Server

data View
  = Private Model.User -- current user
  | Public
      Model.User -- other user
      Bool -- other user is followed?

instance ToHtml View where
  toHtml view =
    case view of
      Private currentUser ->
        profileTemplate currentUser $
          a_
            [ class_ "btn btn-sm btn-outline-secondary action-btn",
              hxBoost_ "true",
              hxTarget_ "#content-slot",
              hxPushUrlSafe_ (Left True),
              hxGetSafe_ settingsLink,
              href_ $ toUrl settingsLink
            ]
            $ do
              i_ [class_ "ion-gear-a"] ""
              " Edit Profile Settings "
      Public otherUser following ->
        profileTemplate otherUser $
          if following
            then toHtml $ UnfollowButton $ Model.userUsername otherUser
            else toHtml $ FollowButton $ Model.userUsername otherUser
    where
      profileTemplate :: Monad m => Model.User -> HtmlT m () -> HtmlT m ()
      profileTemplate (Model.User bio email _ imageUrl username) action =
      profileTemplate (Model.User bio email userID imageUrl username) action =
        div_ [class_ "profile-page"] $ do
          div_ [class_ "user-info"] $
            div_ [class_ "container"] $
              div_ [class_ "row"] $
                div_ [class_ "col-xs-12 col-md-10 offset-md-1"] $ do
                  img_ [src_ imageUrl, class_ "user-img"]
                  h4_ $ toHtml username
                  p_ $ toHtml bio
                  action

          div_ [class_ "container"] $
            div_ [class_ "row"] $
              div_ [class_ "col-xs-12 col-md-10 offset-md-1"] $ do
                toHtml $ Feed True [("My Articles", "", True), ("Favorited Articles", "", False)] []
              div_ [id_ "feed", class_ "col-xs-12 col-md-10 offset-md-1"] $ do
                toHtml $ Feed True [("My Articles", authorFeedLink userID, True), ("Favorited Articles", favoritesFeedLink userID, False)] []
  toHtmlRaw = toHtml

type Route = "profile" :> :> HXRequest :> Capture "username" Text :> Get '[HTML] (Template.Partial View)

handler :: Maybe Text -> App (Template.Partial View)
handler hxReq = pure $ case hxReq of
  Just "true" -> Template.NotWrapped $ View Nothing []
  _ -> Template.Wrapped Nothing $ View Nothing []
