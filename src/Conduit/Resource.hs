{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Conduit.Resource where

import qualified Conduit.Model as Model
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

-- VIEWS START --
data Partial a = Wrapped (Maybe Model.User) a | NotWrapped a

toUrl link = "/" <> toUrlPiece link

data Navbar = Navbar Bool | LoggedInNavbar Bool Model.User

instance ToHtml Navbar where
  toHtml (Navbar isOob) = do
    nav_
      [ id_ "navbar",
        class_ "navbar navbar-light",
        hxBoost_ "true",
        hxTarget_ "#content-slot",
        hxPushUrlSafe_ (Left True),
        if isOob then hxSwapOob_ "true" else class_ ""
      ]
      $ div_ [class_ "container"] $ do
        a_ [class_ "navbar-brand", hxGetSafe_ homeLink, href_ $ toUrl homeLink] "conduit"
        ul_ [class_ "nav navbar-nav pull-xs-right"] $ do
          li_ [class_ "nav-item"] $
            a_ [class_ "nav-link", hxGetSafe_ homeLink, href_ $ toUrl homeLink, [__| on currentUrlChanged log 50000 |]] "Home"
          li_ [class_ "nav-item"] $ a_ [class_ "nav-link", hxGetSafe_ signInFormLink, href_ $ toUrl signInFormLink] "Sign in"
          li_ [class_ "nav-item"] $ a_ [class_ "nav-link", hxGetSafe_ signUpFormLink, href_ $ toUrl signUpFormLink] "Sign up"
  toHtml (LoggedInNavbar isOob (Model.User bio email imageUrl username)) = do
    nav_
      [ id_ "navbar",
        class_ "navbar navbar-light",
        hxBoost_ "true",
        hxTarget_ "#content-slot",
        hxPushUrlSafe_ (Left True),
        if isOob then hxSwapOob_ "true" else class_ ""
      ]
      $ div_ [class_ "container"] $ do
        a_
          [ class_ "navbar-brand",
            hxGetSafe_ homeLink,
            href_ $ toUrl homeLink
          ]
          "conduit"
        ul_ [class_ "nav navbar-nav pull-xs-right"] $ do
          li_ [class_ "nav-item"] $
            a_
              [ class_ "nav-link",
                hxGetSafe_ homeLink,
                href_ $ toUrl homeLink
              ]
              "Home"
          li_ [class_ "nav-item"] $
            a_
              [ class_ "nav-link",
                href_ ""
              ]
              $ do
                i_ [class_ "ion-compose"] ""
                "New Article"
          li_ [class_ "nav-item"] $
            a_
              [ class_ "nav-link",
                href_ ""
              ]
              $ do
                i_ [class_ "ion-gear-a"] ""
                "Settings"
          li_ [class_ "nav-item"] $
            a_ [class_ "nav-link", href_ ""] $ do
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
        useHtmxExtension "json-enc"
        useHyperscript
        [_hs|
          on htmx:pushedIntoHistory(detail) set $currentUrl to detail.path
          then trigger currentUrlChanged
          then log $currentUrl
          -- then trigger currentUrlChanged
        |]
      body_ $ do
        -- NAVBAR
        case mbUser of
          Nothing -> toHtml $ Navbar False
          Just user -> toHtml $ LoggedInNavbar False user

        div_ [id_ "content-slot"] $ toHtml content

        -- FOOTER
        toHtml Footer
  toHtml (NotWrapped a) = toHtml a
  toHtmlRaw = toHtml

data Home = Home | LoggedInHome Model.User

instance ToHtml Home where
  toHtml Home =
    div_ [class_ "home-page"] $ do
      div_ [class_ "banner"] $
        div_ [class_ "container"] $ do
          h1_ [class_ "logo-font"] "conduit"
          p_ "A place to share your knowledge."
      div_ [class_ "container page"] $
        div_ [class_ "row"] $ do
          div_ [class_ "col-md-9"] $ do
            div_ [class_ "feed-toggle"] $
              ul_ [class_ "nav nav-pills outline-active"] $ do
                li_ [class_ "nav-item"] $ a_ [class_ "nav-link active", href_ ""] "Global Feed"
            div_ [class_ "article-preview"] $ do
              div_ [class_ "article-meta"] $ do
                a_ [href_ "profile.html"] $ img_ [src_ "http://i.imgur.com/Qr71crq.jpg"]
                div_ [class_ "info"] $ do
                  a_ [href_ "", class_ "author"] "Eric Simons"
                  span_ [class_ "date"] "January 20th"
                button_ [class_ "btn btn-outline-primary btn-sm pull-xs-right"] $ do
                  i_ [class_ "ion-heart"] ""
                  " 29\n                        "
              a_ [href_ "", class_ "preview-link"] $ do
                h1_ "How to build webapps that scale"
                p_ "This is the description for the post."
                span_ "Read more..."
            div_ [class_ "article-preview"] $ do
              div_ [class_ "article-meta"] $ do
                a_ [href_ "profile.html"] $ img_ [src_ "http://i.imgur.com/N4VcUeJ.jpg"]
                div_ [class_ "info"] $ do
                  a_ [href_ "", class_ "author"] "Albert Pai"
                  span_ [class_ "date"] "January 20th"
                button_ [class_ "btn btn-outline-primary btn-sm pull-xs-right"] $ do
                  i_ [class_ "ion-heart"] ""
                  " 32\n                        "
              a_ [href_ "", class_ "preview-link"] $ do
                h1_ "The song you won't ever stop singing. No matter how hard you try."
                p_ "This is the description for the post."
                span_ "Read more..."
          div_ [class_ "col-md-3"] $
            div_ [class_ "sidebar"] $ do
              p_ "Popular Tags"
              div_ [class_ "tag-list"] $ do
                a_ [href_ "", class_ "tag-pill tag-default"] "programming"
                a_ [href_ "", class_ "tag-pill tag-default"] "javascript"
                a_ [href_ "", class_ "tag-pill tag-default"] "emberjs"
                a_ [href_ "", class_ "tag-pill tag-default"] "angularjs"
                a_ [href_ "", class_ "tag-pill tag-default"] "react"
                a_ [href_ "", class_ "tag-pill tag-default"] "mean"
                a_ [href_ "", class_ "tag-pill tag-default"] "node"
                a_ [href_ "", class_ "tag-pill tag-default"] "rails"
  toHtml (LoggedInHome user) =
    div_ [class_ "home-page"] $ do
      div_ [class_ "container page"] $
        div_ [class_ "row"] $ do
          div_ [class_ "col-md-9"] $ do
            div_ [class_ "feed-toggle"] $
              ul_ [class_ "nav nav-pills outline-active"] $ do
                li_ [class_ "nav-item"] $ a_ [class_ "nav-link active", href_ ""] "Your Feed"
                li_ [class_ "nav-item"] $ a_ [class_ "nav-link disabled", href_ ""] "Global Feed"
            div_ [class_ "article-preview"] $ do
              div_ [class_ "article-meta"] $ do
                a_ [href_ ""] $ img_ [src_ "http://i.imgur.com/Qr71crq.jpg"]
                div_ [class_ "info"] $ do
                  a_ [href_ "", class_ "author"] "Eric Simons"
                  span_ [class_ "date"] "January 20th"
                button_ [class_ "btn btn-outline-primary btn-sm pull-xs-right"] $ do
                  i_ [class_ "ion-heart"] ""
                  " 29\n                        "
              a_ [href_ "", class_ "preview-link"] $ do
                h1_ "How to build webapps that scale"
                p_ "This is the description for the post."
                span_ "Read more..."
            div_ [class_ "article-preview"] $ do
              div_ [class_ "article-meta"] $ do
                a_ [href_ "profile.html"] $ img_ [src_ "http://i.imgur.com/N4VcUeJ.jpg"]
                div_ [class_ "info"] $ do
                  a_ [href_ "", class_ "author"] "Albert Pai"
                  span_ [class_ "date"] "January 20th"
                button_ [class_ "btn btn-outline-primary btn-sm pull-xs-right"] $ do
                  i_ [class_ "ion-heart"] ""
                  " 32\n                        "
              a_ [href_ "", class_ "preview-link"] $ do
                h1_ "The song you won't ever stop singing. No matter how hard you try."
                p_ "This is the description for the post."
                span_ "Read more..."
          div_ [class_ "col-md-3"] $
            div_ [class_ "sidebar"] $ do
              p_ "Popular Tags"
              div_ [class_ "tag-list"] $ do
                a_ [href_ "", class_ "tag-pill tag-default"] "programming"
                a_ [href_ "", class_ "tag-pill tag-default"] "javascript"
                a_ [href_ "", class_ "tag-pill tag-default"] "emberjs"
                a_ [href_ "", class_ "tag-pill tag-default"] "angularjs"
                a_ [href_ "", class_ "tag-pill tag-default"] "react"
                a_ [href_ "", class_ "tag-pill tag-default"] "mean"
                a_ [href_ "", class_ "tag-pill tag-default"] "node"
                a_ [href_ "", class_ "tag-pill tag-default"] "rails"
  toHtmlRaw = toHtml

data SignUpForm = SignUpForm Model.SignUpForm [Text]

instance ToHtml SignUpForm where
  toHtml (SignUpForm (Model.SignUpForm email password username) errors) =
    div_ [class_ "auth-page"] $
      div_ [class_ "container page"] $
        div_ [class_ "row"] $
          div_ [class_ "col-md-6 offset-md-3 col-xs-12"] $ do
            h1_ [class_ "text-xs-center"] "Sign up"
            p_ [class_ "text-xs-center"] $ a_ [hxBoost_ "true", hxTarget_ "#content-slot", hxPushUrlSafe_ (Left True), href_ $ toUrl signInFormLink] "Have an account?"
            case errors of
              [] -> ""
              errors' -> ul_ [class_ "error-messages"] $ mapM_ (li_ [] . toHtml) errors'
            form_ [hxPost_ $ toUrl signUpFormSubmitLink, hxExt_ "json-enc", hxTarget_ "#content-slot"] $ do
              fieldset_ [class_ "form-group"] $
                input_ [class_ "form-control form-control-lg", type_ "text", name_ "signUpFormUsername", placeholder_ "Username", value_ username]
              fieldset_ [class_ "form-group"] $
                input_ [class_ "form-control form-control-lg", type_ "text", name_ "signUpFormEmail", placeholder_ "Email", value_ email]
              fieldset_ [class_ "form-group"] $
                input_ [class_ "form-control form-control-lg", type_ "password", name_ "signUpFormPassword", placeholder_ "Password", value_ password]
              button_ [class_ "btn btn-lg btn-primary pull-xs-right"] "Sign up"
  toHtmlRaw = toHtml

data SignInForm = SignInForm Model.SignInForm [Text]

instance ToHtml SignInForm where
  toHtml (SignInForm (Model.SignInForm email password) errors) =
    div_ [class_ "auth-page"] $
      div_ [class_ "container page"] $
        div_ [class_ "row"] $
          div_ [class_ "col-md-6 offset-md-3 col-xs-12"] $ do
            h1_ [class_ "text-xs-center"] "Sign in"
            p_ [class_ "text-xs-center"] $ a_ [hxBoost_ "true", hxTarget_ "#content-slot", hxPushUrlSafe_ (Left True), href_ $ toUrl signUpFormLink] "Need an account?"
            case errors of
              [] -> ""
              errors' -> ul_ [class_ "error-messages"] $ mapM_ (li_ [] . toHtml) errors'
            form_ [hxPost_ $ toUrl signInFormSubmitLink, hxExt_ "json-enc", hxTarget_ "#content-slot"] $ do
              fieldset_ [class_ "form-group"] $ input_ [class_ "form-control form-control-lg", type_ "text", name_ "signInFormEmail", placeholder_ "Email", value_ email]
              fieldset_ [class_ "form-group"] $ input_ [class_ "form-control form-control-lg", type_ "password", name_ "signInFormPassword", placeholder_ "Password", value_ password]
              button_ [class_ "btn btn-lg btn-primary pull-xs-right"] "Sign in"
  toHtmlRaw = toHtml

data Profile = Profile Model.User

instance ToHtml Profile where
  toHtml (Profile _) =
    div_ [class_ "profile-page"] $ do
      div_ [class_ "user-info"] $
        div_ [class_ "container"] $
          div_ [class_ "row"] $
            div_ [class_ "col-xs-12 col-md-10 offset-md-1"] $ do
              img_ [src_ "http://i.imgur.com/Qr71crq.jpg", class_ "user-img"]
              h4_ "Eric Simons"
              p_ "\n                        Cofounder @GoThinkster, lived in Aol's HQ for a few months, kinda looks like Peeta from the\n                        Hunger Games\n                    "
              button_ [class_ "btn btn-sm btn-outline-secondary action-btn"] $ do
                i_ [class_ "ion-plus-round"] ""
                "\n                        \160\n                        Follow Eric Simons\n                    "
      div_ [class_ "container"] $
        div_ [class_ "row"] $
          div_ [class_ "col-xs-12 col-md-10 offset-md-1"] $ do
            div_ [class_ "articles-toggle"] $
              ul_ [class_ "nav nav-pills outline-active"] $ do
                li_ [class_ "nav-item"] $ a_ [class_ "nav-link active", href_ ""] "My Articles"
                li_ [class_ "nav-item"] $ a_ [class_ "nav-link", href_ ""] "Favorited Articles"
            div_ [class_ "article-preview"] $ do
              div_ [class_ "article-meta"] $ do
                a_ [href_ ""] $ img_ [src_ "http://i.imgur.com/Qr71crq.jpg"]
                div_ [class_ "info"] $ do
                  a_ [href_ "", class_ "author"] "Eric Simons"
                  span_ [class_ "date"] "January 20th"
                button_ [class_ "btn btn-outline-primary btn-sm pull-xs-right"] $ do
                  i_ [class_ "ion-heart"] ""
                  " 29\n                        "
              a_ [href_ "", class_ "preview-link"] $ do
                h1_ "How to build webapps that scale"
                p_ "This is the description for the post."
                span_ "Read more..."
            div_ [class_ "article-preview"] $ do
              div_ [class_ "article-meta"] $ do
                a_ [href_ ""] $ img_ [src_ "http://i.imgur.com/N4VcUeJ.jpg"]
                div_ [class_ "info"] $ do
                  a_ [href_ "", class_ "author"] "Albert Pai"
                  span_ [class_ "date"] "January 20th"
                button_ [class_ "btn btn-outline-primary btn-sm pull-xs-right"] $ do
                  i_ [class_ "ion-heart"] ""
                  " 32\n                        "
              a_ [href_ "", class_ "preview-link"] $ do
                h1_ "The song you won't ever stop singing. No matter how hard you try."
                p_ "This is the description for the post."
                span_ "Read more..."
                ul_ [class_ "tag-list"] $ do
                  li_ [class_ "tag-default tag-pill tag-outline"] "Music"
                  li_ [class_ "tag-default tag-pill tag-outline"] "Song"
  toHtmlRaw = toHtml

data Settings = Settings

instance ToHtml Settings where
  toHtml Settings =
    div_ [class_ "settings-page"] $
      div_ [class_ "container page"] $
        div_ [class_ "row"] $
          div_ [class_ "col-md-6 offset-md-3 col-xs-12"] $ do
            h1_ [class_ "text-xs-center"] "Your Settings"
            form_ $
              fieldset_ $ do
                fieldset_ [class_ "form-group"] $ input_ [class_ "form-control", type_ "text", placeholder_ "URL of profile picture"]
                fieldset_ [class_ "form-group"] $ input_ [class_ "form-control form-control-lg", type_ "text", placeholder_ "Your Name"]
                fieldset_ [class_ "form-group"] $ textarea_ [class_ "form-control form-control-lg", rows_ "8", placeholder_ "Short bio about you"] ""
                fieldset_ [class_ "form-group"] $ input_ [class_ "form-control form-control-lg", type_ "text", placeholder_ "Email"]
                fieldset_ [class_ "form-group"] $ input_ [class_ "form-control form-control-lg", type_ "password", placeholder_ "Password"]
                button_ [class_ "btn btn-lg btn-primary pull-xs-right"] "\n                            Update Settings\n                        "
  toHtmlRaw = toHtml

data Editor = Editor

instance ToHtml Editor where
  toHtml Editor =
    div_ [class_ "editor-page"] $
      div_ [class_ "container page"] $
        div_ [class_ "row"] $
          div_ [class_ "col-md-10 offset-md-1 col-xs-12"] $
            form_ $
              fieldset_ $ do
                fieldset_ [class_ "form-group"] $ input_ [type_ "text", class_ "form-control form-control-lg", placeholder_ "Article Title"]
                fieldset_ [class_ "form-group"] $ input_ [type_ "text", class_ "form-control", placeholder_ "What's this article about?"]
                fieldset_ [class_ "form-group"] $ textarea_ [class_ "form-control", rows_ "8", placeholder_ "Write your article (in markdown)"] ""
                fieldset_ [class_ "form-group"] $ do
                  input_ [type_ "text", class_ "form-control", placeholder_ "Enter tags"]
                  div_ [class_ "tag-list"] ""
                button_ [class_ "btn btn-lg pull-xs-right btn-primary", type_ "button"] "\n                            Publish Article\n                        "
  toHtmlRaw = toHtml

data Article = Article

instance ToHtml Article where
  toHtml Article =
    div_ [class_ "article-page"] $ do
      div_ [class_ "banner"] $
        div_ [class_ "container"] $ do
          h1_ "How to build webapps that scale"
          div_ [class_ "article-meta"] $ do
            a_ [href_ ""] $ img_ [src_ "http://i.imgur.com/Qr71crq.jpg"]
            div_ [class_ "info"] $ do
              a_ [href_ "", class_ "author"] "Eric Simons"
              span_ [class_ "date"] "January 20th"
            button_ [class_ "btn btn-sm btn-outline-secondary"] $ do
              i_ [class_ "ion-plus-round"] ""
              "\n                    \160\n                    Follow Eric Simons "
              span_ [class_ "counter"] "(10)"
            button_ [class_ "btn btn-sm btn-outline-primary"] $ do
              i_ [class_ "ion-heart"] ""
              "\n                    \160\n                    Favorite Post "
              span_ [class_ "counter"] "(29)"
      div_ [class_ "container page"] $ do
        div_ [class_ "row article-content"] $
          div_ [class_ "col-md-12"] $ do
            p_ "\n                    Web development technologies have evolved at an incredible clip over the past few years.\n                "
            h2_ [id_ "introducing-ionic"] "Introducing RealWorld."
            p_ "It's a great solution for learning how other frameworks work."
        hr_ []
        div_ [class_ "article-actions"] $
          div_ [class_ "article-meta"] $ do
            a_ [href_ "profile.html"] $ img_ [src_ "http://i.imgur.com/Qr71crq.jpg"]
            div_ [class_ "info"] $ do
              a_ [href_ "", class_ "author"] "Eric Simons"
              span_ [class_ "date"] "January 20th"
            button_ [class_ "btn btn-sm btn-outline-secondary"] $ do
              i_ [class_ "ion-plus-round"] ""
              "\n                    \160\n                    Follow Eric Simons\n                "
            button_ [class_ "btn btn-sm btn-outline-primary"] $ do
              i_ [class_ "ion-heart"] ""
              "\n                    \160\n                    Favorite Post "
              span_ [class_ "counter"] "(29)"
        div_ [class_ "row"] $
          div_ [class_ "col-xs-12 col-md-8 offset-md-2"] $ do
            form_ [class_ "card comment-form"] $ do
              div_ [class_ "card-block"] $ textarea_ [class_ "form-control", placeholder_ "Write a comment...", rows_ "3"] ""
              div_ [class_ "card-footer"] $ do
                img_ [src_ "http://i.imgur.com/Qr71crq.jpg", class_ "comment-author-img"]
                button_ [class_ "btn btn-sm btn-primary"] "\n                            Post Comment\n                        "
            div_ [class_ "card"] $ do
              div_ [class_ "card-block"] $ p_ [class_ "card-text"] "With supporting text below as a natural lead-in to additional content."
              div_ [class_ "card-footer"] $ do
                a_ [href_ "", class_ "comment-author"] $ img_ [src_ "http://i.imgur.com/Qr71crq.jpg", class_ "comment-author-img"]
                a_ [href_ "", class_ "comment-author"] "Jacob Schmidt"
                span_ [class_ "date-posted"] "Dec 29th"
            div_ [class_ "card"] $ do
              div_ [class_ "card-block"] $ p_ [class_ "card-text"] "With supporting text below as a natural lead-in to additional content."
              div_ [class_ "card-footer"] $ do
                a_ [href_ "", class_ "comment-author"] $ img_ [src_ "http://i.imgur.com/Qr71crq.jpg", class_ "comment-author-img"]
                a_ [href_ "", class_ "comment-author"] "Jacob Schmidt"
                span_ [class_ "date-posted"] "Dec 29th"
                span_ [class_ "mod-options"] $ do
                  i_ [class_ "ion-edit"] ""
                  i_ [class_ "ion-trash-a"] ""
  toHtmlRaw = toHtml

data SignUpResponse
  = SignUpFailure Model.SignUpForm [Text]
  | SignUpSuccess Model.User

instance ToHtml SignUpResponse where
  toHtml (SignUpFailure signUpForm errors) = toHtml $ SignUpForm signUpForm errors
  toHtml (SignUpSuccess user) = do
    toHtml $ LoggedInHome user
    toHtml $ LoggedInNavbar True user
  toHtmlRaw = toHtml

data SignInResponse
  = SignInFailure Model.SignInForm [Text]
  | SignInSuccess Model.User

instance ToHtml SignInResponse where
  toHtml (SignInFailure signInForm errors) = toHtml $ SignInForm signInForm errors
  toHtml (SignInSuccess user) = do
    toHtml $ LoggedInHome user
    toHtml $ LoggedInNavbar True user
  toHtmlRaw = toHtml

-- VIEWS END --

-- ROUTES START --

type HomeRoute = HXRequest :> Get '[HTML] (Partial Home)

type SignUpFormRoute = "register" :> HXRequest :> Get '[HTML] (Partial SignUpForm)

type SignInFormRoute = "login" :> HXRequest :> Get '[HTML] (Partial SignInForm)

type SignUpFormSubmitRoute =
  "sign-up"
    :> ReqBody '[JSON] Model.SignUpForm
    :> Post '[HTML] (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] SignUpResponse)

type SignInFormSubmitRoute =
  "sign-in"
    :> ReqBody '[JSON] Model.SignInForm
    :> Post '[HTML] (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] SignInResponse)

type UnprotectedRoutes =
  HomeRoute
    :<|> SignUpFormRoute
    :<|> SignInFormRoute
    :<|> SignUpFormSubmitRoute
    :<|> SignInFormSubmitRoute

type ProtectedRoutes = HomeRoute

type Routes =
  (Auth '[Cookie] Model.User :> ProtectedRoutes)
    :<|> UnprotectedRoutes

-- ROUTES END --

-- LINKS START --

proxy :: forall a. Proxy a
proxy = Proxy

getLink ::
  forall endpoint.
  (IsElem endpoint Routes, HasLink endpoint) =>
  Proxy endpoint ->
  MkLink endpoint Link
getLink = safeLink $ proxy @Routes

homeLink = getLink $ proxy @HomeRoute

signInFormLink = getLink $ proxy @SignInFormRoute

signInFormSubmitLink = getLink $ proxy @SignInFormSubmitRoute

signUpFormLink = getLink $ proxy @SignUpFormRoute

signUpFormSubmitLink = getLink $ proxy @SignUpFormSubmitRoute

-- LINKS END --
