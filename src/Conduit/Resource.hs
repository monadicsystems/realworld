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

toUrl :: ToHttpApiData a => a -> Text
toUrl link = "/" <> toUrlPiece link

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
        a_ [class_ "navbar-brand", hxGetSafe_ homeLink, href_ $ toUrl homeLink] "conduit"
        ul_ [class_ "nav navbar-nav pull-xs-right"] $ do
          li_ [class_ "nav-item"] $
            a_ [class_ "nav-link", hxGetSafe_ homeLink, href_ $ toUrl homeLink] "Home"
          case mbUser of
            Nothing -> do
              li_ [class_ "nav-item"] $ a_ [class_ "nav-link", hxGetSafe_ signInFormLink, href_ $ toUrl signInFormLink] "Sign in"
              li_ [class_ "nav-item"] $ a_ [class_ "nav-link", hxGetSafe_ signUpFormLink, href_ $ toUrl signUpFormLink] "Sign up"
            Just (Model.User bio email _ imageUrl username) -> do
              li_ [class_ "nav-item"] $
                a_
                  [ class_ "nav-link",
                    hxGetSafe_ editorLink,
                    href_ $ toUrl editorLink
                  ]
                  $ do
                    i_ [class_ "ion-compose"] ""
                    "New Article"
              li_ [class_ "nav-item"] $
                a_
                  [ class_ "nav-link",
                    hxGetSafe_ settingsLink,
                    href_ $ toUrl settingsLink
                  ]
                  $ do
                    i_ [class_ "ion-gear-a"] ""
                    "Settings"
              li_ [class_ "nav-item"] $
                a_ [class_ "nav-link", href_ . toUrl . profileLink $ "@" <> username] $ do
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
        -- useHtmxExtension "json-enc"
        useHyperscript
        [_hs|
          on htmx:pushedIntoHistory(detail) set $currentUrl to detail.path
          then trigger currentUrlChanged
          then log $currentUrl
        |]
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
      [(Model.User, Model.Article, [Model.Tag])] -- List of author article tags tuple

instance ToHtml Feed where
  toHtml (Feed isProfile pills articles) = do
    div_ [class_ $ if isProfile then "articles-toggle" else "feed-toggle"] $
      ul_ [class_ "nav nav-pills outline-active"] $ do
        forM_ pills $ \(name, link, isActive) -> do
          li_ [class_ "nav-item"] $
            a_ [class_ $ "nav-link " <> if isActive then "active" else "disabled", href_ link] $ toHtml name
    -- ARTICLES START
    forM_ articles $ \(Model.User{..}, Model.Article{..}, tags) ->
      div_ [class_ "article-preview"] $ do
        div_ [class_ "article-meta"] $ do
          a_ [href_ . toUrl . profileLink $ "@" <> userUsername] $ img_ [src_ userImageUrl]
          div_ [class_ "info"] $ do
            a_ [href_ . toUrl . profileLink $ "@" <> userUsername, class_ "author"] $ toHtml userUsername
            span_ [class_ "date"] $ toHtml ("January 20th" :: Text)-- articleCreatedAt
          button_ [class_ "btn btn-outline-primary btn-sm pull-xs-right"] $ do
            i_ [class_ "ion-heart"] ""
            toHtml $ show articleFavorites
        a_ [href_ "" {- slugify article title -}, class_ "preview-link"] $ do
          h1_ $ toHtml articleTitle
          p_ $ toHtml articleDescription
          span_ $ toHtml ("Read more..." :: Text)
          ul_ [class_ "tag-list"] $ forM_ tags $ \(Model.Tag t) ->
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

data Home = Home (Maybe Model.User)

instance ToHtml Home where
  toHtml (Home mbUser) =
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
          div_ [id_ "feeds", class_ "col-md-9"] $
            toHtml $ case mbUser of
              Nothing -> Feed False [("Global Feed", "", True)] []
              Just _ ->
                Feed
                  False
                  [("Your Feed", "", True), ("Global Feed", "", False)]
                  []

          -- TAGS
          div_ [id_ "tags", class_ "col-md-3"] $ toHtml $ Tagbar []
  toHtmlRaw = toHtml

data SignUpForm = SignUpForm (Maybe Model.SignUpForm) [Text]

instance ToHtml SignUpForm where
  toHtml (SignUpForm mbSignUpForm errors) =
    div_ [class_ "auth-page"] $
      div_ [class_ "container page"] $
        div_ [class_ "row"] $
          div_ [class_ "col-md-6 offset-md-3 col-xs-12"] $ do
            h1_ [class_ "text-xs-center"] "Sign up"
            p_ [class_ "text-xs-center"] $ a_ [hxBoost_ "true", hxTarget_ "#content-slot", hxPushUrlSafe_ (Left True), href_ $ toUrl signInFormLink] "Have an account?"
            case errors of
              [] -> ""
              errors' -> ul_ [class_ "error-messages"] $ mapM_ (li_ [] . toHtml) errors'
            form_ [hxPost_ $ toUrl signUpFormSubmitLink, hxTarget_ "#content-slot"] $ do
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "text",
                    name_ "signUpFormUsername",
                    placeholder_ "Username",
                    value_ $ maybe "" Model.signUpFormUsername mbSignUpForm
                  ]
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "text",
                    name_ "signUpFormEmail",
                    placeholder_ "Email",
                    value_ $ maybe "" Model.signUpFormEmail mbSignUpForm
                  ]
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "password",
                    name_ "signUpFormPassword",
                    placeholder_ "Password",
                    value_ $ maybe "" Model.signUpFormPassword mbSignUpForm
                  ]
              button_ [class_ "btn btn-lg btn-primary pull-xs-right"] "Sign up"
  toHtmlRaw = toHtml

data SignInForm = SignInForm (Maybe Model.SignInForm) [Text]

instance ToHtml SignInForm where
  toHtml (SignInForm mbSignInForm errors) =
    div_ [class_ "auth-page"] $
      div_ [class_ "container page"] $
        div_ [class_ "row"] $
          div_ [class_ "col-md-6 offset-md-3 col-xs-12"] $ do
            h1_ [class_ "text-xs-center"] "Sign in"
            p_ [class_ "text-xs-center"] $ a_ [hxBoost_ "true", hxTarget_ "#content-slot", hxPushUrlSafe_ (Left True), href_ $ toUrl signUpFormLink] "Need an account?"
            case errors of
              [] -> ""
              errors' -> ul_ [class_ "error-messages"] $ mapM_ (li_ [] . toHtml) errors'
            form_ [hxPost_ $ toUrl signInFormSubmitLink, hxTarget_ "#content-slot"] $ do
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "text",
                    name_ "signInFormEmail",
                    placeholder_ "Email",
                    value_ $ maybe "" Model.signInFormEmail mbSignInForm
                  ]
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "password",
                    name_ "signInFormPassword",
                    placeholder_ "Password",
                    value_ $ maybe "" Model.signInFormPassword mbSignInForm
                  ]
              button_ [class_ "btn btn-lg btn-primary pull-xs-right"] "Sign in"
  toHtmlRaw = toHtml

data Profile
  = PrivateProfile Model.User -- current user
  | PublicProfile
      Model.User -- other user
      Bool -- other user is followed?

instance ToHtml Profile where
  toHtml profile =
    case profile of
      PrivateProfile currentUser ->
        profileTemplate currentUser $
          a_
            [ class_ "btn btn-sm btn-outline-secondary action-btn",
              hxGetSafe_ settingsLink,
              href_ $ toUrl settingsLink
            ]
            $ do
              i_ [class_ "ion-gear-a"] ""
              "Edit Profile Settings"
      PublicProfile otherUser following ->
        profileTemplate otherUser $
          if following
            then toHtml $ UnfollowButton $ Model.userUsername otherUser
            else toHtml $ FollowButton $ Model.userUsername otherUser
    where
      profileTemplate :: Monad m => Model.User -> HtmlT m () -> HtmlT m ()
      profileTemplate (Model.User bio email _ imageUrl username) action =
        div_ [class_ "profile-page"] $ do
          div_ [class_ "user-info"] $
            div_ [class_ "container"] $
              div_ [class_ "row"] $
                div_ [class_ "col-xs-12 col-md-10 offset-md-1"] $ do
                  img_ [src_ imageUrl, class_ "user-img"]
                  h4_ $ toHtml username
                  p_ $ toHtml bio

          div_ [class_ "container"] $
            div_ [class_ "row"] $
              div_ [class_ "col-xs-12 col-md-10 offset-md-1"] $ do
                toHtml $ Feed True [("My Articles", "", True), ("Favorited Articles", "", False)] []
  toHtmlRaw = toHtml

data Settings = Settings Model.User

instance ToHtml Settings where
  toHtml (Settings (Model.User bio email userID imageUrl username)) =
    div_ [class_ "settings-page"] $
      div_ [class_ "container page"] $
        div_ [class_ "row"] $
          div_ [class_ "col-md-6 offset-md-3 col-xs-12"] $ do
            h1_ [class_ "text-xs-center"] "Your Settings"
            form_ $ do
              fieldset_ $ do
                fieldset_ [class_ "form-group"] $
                  input_
                    [ class_ "form-control",
                      type_ "text",
                      placeholder_ "URL of profile picture",
                      value_ imageUrl
                    ]
                fieldset_ [class_ "form-group"] $
                  input_
                    [ class_ "form-control form-control-lg",
                      type_ "text",
                      placeholder_ "Your Name",
                      value_ username
                    ]
                fieldset_ [class_ "form-group"] $
                  textarea_
                    [ class_ "form-control form-control-lg",
                      rows_ "8",
                      placeholder_ "Short bio about you"
                    ]
                    $ toHtml bio
                fieldset_ [class_ "form-group"] $
                  input_
                    [ class_ "form-control form-control-lg",
                      type_ "text",
                      placeholder_ "Email",
                      value_ email
                    ]
                fieldset_ [class_ "form-group"] $
                  input_
                    [ class_ "form-control form-control-lg",
                      type_ "password",
                      placeholder_ "New Password"
                    ]
                button_ [class_ "btn btn-lg btn-primary pull-xs-right"] "Update Settings"
              hr_ []
              button_ [ class_ "btn btn-outline-danger" ] $ "Or click here to logout."
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
                button_ [class_ "btn btn-lg pull-xs-right btn-primary", type_ "button"] "Publish Article"
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
  toHtml (SignUpFailure signUpForm errors) = toHtml $ SignUpForm (Just signUpForm) errors
  toHtml (SignUpSuccess user) = do
    toHtml $ Home $ Just user
    toHtml $ Navbar (Just user) True
  toHtmlRaw = toHtml

data SignInResponse
  = SignInFailure Model.SignInForm [Text]
  | SignInSuccess Model.User

instance ToHtml SignInResponse where
  toHtml (SignInFailure signInForm errors) = toHtml $ SignInForm (Just signInForm) errors
  toHtml (SignInSuccess user) = do
    toHtml $ Home $ Just user
    toHtml $ Navbar (Just user) True
  toHtmlRaw = toHtml

data FollowButton = FollowButton Text

instance ToHtml FollowButton where
  toHtml (FollowButton followee) =
    button_
      [ class_ "btn btn-sm btn-outline-secondary action-btn",
        hxPostSafe_ followLink,
        hxSwap_ "outerHTML",
        hxVals_ $ "{\"followFormTarget\": \"" <> followee <> "\"}"
      ]
      $ do
        i_ [class_ "ion-plus-round"] ""
        "Follow " <> toHtml followee
  toHtmlRaw = toHtml

data UnfollowButton = UnfollowButton Text

instance ToHtml UnfollowButton where
  toHtml (UnfollowButton unfollowee) =
    button_
      [ class_ "btn btn-sm btn-outline-secondary action-btn",
        hxPostSafe_ unfollowLink,
        hxSwap_ "outerHTML",
        hxVals_ $ "{\"unfollowFormTarget\": \"" <> unfollowee <> "\"}"
      ]
      $ do
        i_ [class_ "ion-plus-round"] ""
        "Unfollow " <> toHtml unfollowee
  toHtmlRaw = toHtml

-- VIEWS END --

-- ROUTES START --
-- TODO: Factor out HXRequest

type SignUpFormRoute = "register" :> HXRequest :> Get '[HTML] (Partial SignUpForm)

type SignInFormRoute = "login" :> HXRequest :> Get '[HTML] (Partial SignInForm)

type SignUpFormSubmitRoute =
  "sign-up"
    :> ReqBody '[FormUrlEncoded] Model.SignUpForm
    :> Post '[HTML] (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] SignUpResponse)

type SignInFormSubmitRoute =
  "sign-in"
    :> ReqBody '[FormUrlEncoded] Model.SignInForm
    :> Post '[HTML] (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] SignInResponse)

type UnprotectedRoutes =
  SignUpFormRoute
    :<|> SignInFormRoute
    :<|> SignUpFormSubmitRoute
    :<|> SignInFormSubmitRoute

-- TODO: REMOVE PARTIALS WHERE POSSIBLE

type HomeRoute = HXRequest :> Get '[HTML] (Partial Home)

type FollowRoute = "follow" :> ReqBody '[FormUrlEncoded] Model.FollowForm :> Post '[HTML] UnfollowButton

type UnfollowRoute = "unfollow" :> ReqBody '[FormUrlEncoded] Model.UnfollowForm :> Post '[HTML] FollowButton

type EditorRoute = "editor" :> HXRequest :> Get '[HTML] (Partial Editor)

type SettingsRoute = "settings" :> HXRequest :> Get '[HTML] (Partial Settings)

type ProfileRoute = "profile" :> HXRequest :> Capture "username" Text :> Get '[HTML] (Partial Profile)

type ProtectedRoutes =
  HomeRoute
    :<|> FollowRoute
    :<|> UnfollowRoute
    :<|> EditorRoute
    :<|> SettingsRoute
    :<|> ProfileRoute

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

signInFormLink :: Link
signInFormLink = getLink $ proxy @SignInFormRoute

signInFormSubmitLink :: Link
signInFormSubmitLink = getLink $ proxy @SignInFormSubmitRoute

signUpFormLink :: Link
signUpFormLink = getLink $ proxy @SignUpFormRoute

signUpFormSubmitLink :: Link
signUpFormSubmitLink = getLink $ proxy @SignUpFormSubmitRoute

homeLink :: Link
homeLink = getLink $ proxy @(Auth '[Cookie] Model.User :> HomeRoute)

profileLink :: Text -> Link
profileLink = getLink $ proxy @(Auth '[Cookie] Model.User :> ProfileRoute)

followLink :: Link
followLink = getLink $ proxy @(Auth '[Cookie] Model.User :> FollowRoute)

unfollowLink :: Link
unfollowLink = getLink $ proxy @(Auth '[Cookie] Model.User :> UnfollowRoute)

editorLink :: Link
editorLink = getLink $ proxy @(Auth '[Cookie] Model.User :> EditorRoute)

settingsLink :: Link
settingsLink = getLink $ proxy @(Auth '[Cookie] Model.User :> SettingsRoute)

-- LINKS END --
