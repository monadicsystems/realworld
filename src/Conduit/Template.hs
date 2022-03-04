{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Conduit.Template where

import qualified Conduit.Model as Model
import Control.Monad.State
import Data.Text
import Lucid
import Lucid.Htmx
import Lucid.Htmx.Servant
import Lucid.Hyperscript (useHyperscript, __, _hs)
import Servant (ToHttpApiData)
import Servant.Links

data Partial a = Wrapped (Maybe Model.User) a | NotWrapped a

data Navbar = Navbar (Maybe Model.User) Bool

instance ToHtml Navbar where
  toHtml (Navbar mbUser isOob) = do
    nav_
      [ id_ "navbar",
        class_ "navbar navbar-light",
        hxBoost_ "true",
        hxTarget_ "#content-slot",
        hxPushUrlSafe_ (Left True),
        if isOob then hxSwapOob_ "true" else class_ ""
      ]
      $ div_ [class_ "container"] $ do
        -- TODO: Does it make sense to use hxGet_ and href_ for boost?
        a_ [class_ "navbar-brand", hxGet_ "", href_ ""] "conduit"
        ul_ [class_ "nav navbar-nav pull-xs-right"] $ do
          li_ [class_ "nav-item"] $
            a_ [class_ "nav-link", hxGet_ "", href_ ""] "Home"
          case mbUser of
            Nothing -> do
              li_ [class_ "nav-item"] $ a_ [class_ "nav-link", hxGet_ "", href_ ""] "Sign in"
              li_ [class_ "nav-item"] $ a_ [class_ "nav-link", hxGet_ "", href_ ""] "Sign up"
            Just (Model.User bio email _ imageUrl username) -> do
              li_ [class_ "nav-item"] $
                a_
                  [ class_ "nav-link",
                    hxGet_ "",
                    href_ ""
                  ]
                  $ do
                    i_ [class_ "ion-compose"] ""
                    "New Article"
              li_ [class_ "nav-item"] $
                a_
                  [ class_ "nav-link",
                    hxGet_ "",
                    href_ ""
                  ]
                  $ do
                    i_ [class_ "ion-gear-a"] ""
                    "Settings"
              li_ [class_ "nav-item"] $
                a_
                  [ class_ "nav-link",
                    href_ ""
                  ]
                  $ do
                    img_ [class_ "user-pic", src_ imageUrl]
                    toHtml $ " " <> username <> " "
  toHtmlRaw = toHtml

data Footer = Footer

instance ToHtml Footer where
  toHtml _ =
    footer_ $
      div_ [class_ "container"] $ do
        a_ [href_ "/", class_ "logo-font"] "conduit"
        span_ [class_ "attribution"] $ do
          "          An interactive learning project from "
          a_ [href_ "https://thinkster.io"] "Thinkster"
          ". Code & design licensed under MIT.        "
  toHtmlRaw = toHtml

instance ToHtml a => ToHtml (Partial a) where
  toHtml (Wrapped mbUser content) =
    html_ $ do
      head_ $ do
        meta_ [charset_ "utf-8"]
        title_ "Conduit"
        link_ [href_ "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css", rel_ "stylesheet", type_ "text/css"]
        link_ [href_ "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic", rel_ "stylesheet", type_ "text/css"]
        link_ [rel_ "stylesheet", href_ "//demo.productionready.io/main.css"]
        useHtmx
        useHyperscript
      body_ $ do
        -- NAVBAR
        toHtml $ Navbar mbUser False

        -- CONTENT
        div_ [id_ "content-slot"] $ toHtml content

        -- FOOTER
        toHtml Footer
  toHtml (NotWrapped a) = toHtml a
  toHtmlRaw = toHtml

data Feed
  = Feed
      Bool -- is personal for user profile?
      [(Text, Text, Bool)] -- Feed nav pill name, link and whether or not it's active or disabled
      [Model.ArticleInfo] -- List of author, article, tags tuple

instance ToHtml Feed where
  toHtml (Feed isProfile pills articles) = do
    div_ [class_ $ if isProfile then "articles-toggle" else "feed-toggle"] $
      ul_ [class_ "nav nav-pills outline-active"] $ do
        forM_ pills $ \(name, link, isActive) -> do
          li_ [class_ "nav-item"] $
            a_ [class_ $ "nav-link " <> if isActive then "active" else "", hxGet_ link, hxTarget_ "#feed"] $ toHtml name
    -- ARTICLES START
    forM_ articles $ \(Model.Article {..}, Model.User {..}, tags) ->
      div_ [class_ "article-preview"] $ do
        div_ [class_ "article-meta"] $ do
          a_ [href_ ""] $ img_ [src_ userImageUrl]
          div_ [class_ "info"] $ do
            a_ [href_ "", class_ "author"] $ toHtml userUsername
            span_ [class_ "date"] $ toHtml ("January 20th" :: Text) -- articleCreatedAt
          button_ [class_ "btn btn-outline-primary btn-sm pull-xs-right"] $ do
            i_ [class_ "ion-heart"] ""
            toHtml $ show articleFavorites
        a_ [href_ "" {- slugify article title -}, class_ "preview-link"] $ do
          h1_ $ toHtml articleTitle
          p_ $ toHtml articleDescription
          span_ $ toHtml ("Read more..." :: Text)
          ul_ [class_ "tag-list"] $
            forM_ tags $ \(Model.Tag t) ->
              li_ [class_ "tag-default tag-pill tag-outline"] $ toHtml t

  -- ARTICLES END
  toHtmlRaw = toHtml

data Tagbar = Tagbar [(Text, Text)] -- name of tag and what it links too, could just be name bcuz same as link

instance ToHtml Tagbar where
  toHtml (Tagbar tags) =
    div_ [class_ "sidebar"] $ do
      p_ "Popular Tags"
      div_ [class_ "tag-list"] $
        forM_ tags $ \(name, link) ->
          a_ [href_ link, class_ "tag-pill tag-default"] $ toHtml name
  toHtmlRaw = toHtml
