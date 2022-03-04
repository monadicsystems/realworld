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

module Conduit.Resource.Read.Home where

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

type Route = MakeRoute Get (Auth '[Cookie] Model.User :> HXRequest) (Template.Partial View)

data View = View (Maybe Model.User)

instance ToHtml View where
  toHtml (View mbUser) =
    div_ [class_ "home-page"] $ do
      case mbUser of
        Nothing -> do
          div_ [class_ "banner"] $
            div_ [class_ "container"] $ do
              h1_ [class_ "logo-font"] "conduit"
              p_ "A place to share your knowledge."
        Just _ -> ""
      div_ [class_ "container page"] $
        div_ [class_ "row"] $ do
          -- FEED
          div_ [id_ "feed", class_ "col-md-9"] $
            toHtml $ case mbUser of
              Nothing -> Template.Feed False [("Global Feed", globalFeedLink, True)] []
              Just Model.User {..} ->
                Template.Feed
                  False
                  [("Your Feed", authorFeedLink userID, True), ("Global Feed", globalFeedLink, False)]
                  []
          -- TAGS
          div_ [id_ "tags", class_ "col-md-3"] $ toHtml $ Template.Tagbar []
  toHtmlRaw = toHtml

handler :: AuthResult Model.User -> Maybe Text -> App (Template.Partial View)
handler (Authenticated user) hxReq =
  case hxReq of
    Just "true" -> pure $ Template.NotWrapped $ View (Just user)
    _ -> pure $ Template.Wrapped (Just user) $ View (Just user)
handler _ hxReq =
  case hxReq of
    Just "true" -> pure $ Template.NotWrapped $ View Nothing
    _ -> pure $ Template.Wrapped Nothing $ View Nothing
