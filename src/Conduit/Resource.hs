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

import Conduit.App
import Conduit.Database
import qualified Conduit.Model as Model
import qualified Conduit.Validate as Validate
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Function ((&))
import Data.Map.Strict (toList)
import Data.Proxy
import Data.Text
import Data.Text.Encoding (encodeUtf8)
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
import Web.Forma (FormResult (..), runForm, showFieldName)

-- HELPERS START --
toUrl :: ToHttpApiData a => a -> Text
toUrl link = "/" <> toUrlPiece link

redirectToRegisterForm = throwError $ err303
  { errHeaders = [("Location", encodeUtf8 $ toUrl registerFormLink)]
  }

proxy :: forall a. Proxy a
proxy = Proxy

getLink ::
  forall endpoint.
  (IsElem endpoint Routes, HasLink endpoint) =>
  Proxy endpoint ->
  MkLink endpoint Link
getLink = safeLink $ proxy @Routes

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
        a_ [class_ "navbar-brand", hxGetSafe_ homeLink, href_ $ toUrl homeLink] "conduit"
        ul_ [class_ "nav navbar-nav pull-xs-right"] $ do
          li_ [class_ "nav-item"] $
            a_ [class_ "nav-link", hxGetSafe_ homeLink, href_ $ toUrl homeLink] "Home"
          case mbUser of
            Nothing -> do
              li_ [class_ "nav-item"] $ a_ [class_ "nav-link", hxGetSafe_ loginFormLink, href_ $ toUrl loginFormLink] "Sign in"
              li_ [class_ "nav-item"] $ a_ [class_ "nav-link", hxGetSafe_ registerFormLink, href_ $ toUrl registerFormLink] "Sign up"
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

-- HELPERS END --

-- HOME START --

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

type HomeRoute = Auth '[Cookie] Model.User :> HXRequest :> Get '[HTML] (Partial Home)

homeHandler :: AuthResult Model.User -> Maybe Text -> App (Partial Home)
homeHandler (Authenticated user) hxReq = pure $ case hxReq of
  Just "true" -> NotWrapped $ Home (Just user)
  _ -> Wrapped (Just user) $ Home (Just user)
homeHandler _ hxReq = pure $ case hxReq of
  Just "true" -> NotWrapped $ Home Nothing
  _ -> Wrapped Nothing $ Home Nothing

-- HOME END --

-- Register Form Start --

data RegisterForm = RegisterForm (Maybe Model.RegisterForm) [Text]

instance ToHtml RegisterForm where
  toHtml (RegisterForm mbRegisterForm errors) =
    div_ [class_ "auth-page"] $
      div_ [class_ "container page"] $
        div_ [class_ "row"] $
          div_ [class_ "col-md-6 offset-md-3 col-xs-12"] $ do
            h1_ [class_ "text-xs-center"] "Sign up"
            p_ [class_ "text-xs-center"] $ a_ [hxBoost_ "true", hxTarget_ "#content-slot", hxPushUrlSafe_ (Left True), href_ $ toUrl loginFormLink] "Have an account?"
            case errors of
              [] -> ""
              errors' -> ul_ [class_ "error-messages"] $ mapM_ (li_ [] . toHtml) errors'
            form_ [hxPost_ $ toUrl registerFormSubmitLink, hxTarget_ "#content-slot"] $ do
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "text",
                    name_ "registerFormUsername",
                    placeholder_ "Username",
                    value_ $ maybe "" Model.registerFormUsername mbRegisterForm
                  ]
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "text",
                    name_ "registerFormEmail",
                    placeholder_ "Email",
                    value_ $ maybe "" Model.registerFormEmail mbRegisterForm
                  ]
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "password",
                    name_ "registerFormPassword",
                    placeholder_ "Password",
                    value_ $ maybe "" Model.registerFormPassword mbRegisterForm
                  ]
              button_ [class_ "btn btn-lg btn-primary pull-xs-right"] "Sign up"
  toHtmlRaw = toHtml

type RegisterFormRoute = "register" :> HXRequest :> Get '[HTML] (Partial RegisterForm)

registerFormHandler :: Maybe Text -> App (Partial RegisterForm)
registerFormHandler hxReq = pure $ case hxReq of
  Just "true" -> NotWrapped $ RegisterForm Nothing []
  _ -> Wrapped Nothing $ RegisterForm Nothing []

-- Register Form End --

-- Login Form Start --

data LoginForm = LoginForm (Maybe Model.LoginForm) [Text]

instance ToHtml LoginForm where
  toHtml (LoginForm mbLoginForm errors) =
    div_ [class_ "auth-page"] $
      div_ [class_ "container page"] $
        div_ [class_ "row"] $
          div_ [class_ "col-md-6 offset-md-3 col-xs-12"] $ do
            h1_ [class_ "text-xs-center"] "Sign in"
            p_ [class_ "text-xs-center"] $ a_ [hxBoost_ "true", hxTarget_ "#content-slot", hxPushUrlSafe_ (Left True), href_ $ toUrl registerFormLink] "Need an account?"
            case errors of
              [] -> ""
              errors' -> ul_ [class_ "error-messages"] $ mapM_ (li_ [] . toHtml) errors'
            form_ [hxPostSafe_ loginFormSubmitLink, hxTarget_ "#content-slot"] $ do
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "text",
                    name_ "loginFormEmail",
                    placeholder_ "Email",
                    value_ $ maybe "" Model.loginFormEmail mbLoginForm
                  ]
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "password",
                    name_ "loginFormPassword",
                    placeholder_ "Password",
                    value_ $ maybe "" Model.loginFormPassword mbLoginForm
                  ]
              button_ [class_ "btn btn-lg btn-primary pull-xs-right"] "Sign in"
  toHtmlRaw = toHtml

type LoginFormRoute = "login" :> HXRequest :> Get '[HTML] (Partial LoginForm)

loginFormHandler :: Maybe Text -> App (Partial LoginForm)
loginFormHandler hxReq = pure $ case hxReq of
  Just "true" -> NotWrapped $ LoginForm Nothing []
  _ -> Wrapped Nothing $ LoginForm Nothing []

-- Login Form End --

-- Profile Start --

data Profile
  = ProfilePrivate Model.User -- current user
  | ProfilePublic
      Model.User -- other user
      Bool -- other user is followed?

instance ToHtml Profile where
  toHtml profile =
    case profile of
      ProfilePrivate currentUser ->
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
      ProfilePublic otherUser following ->
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
                  action

          div_ [class_ "container"] $
            div_ [class_ "row"] $
              div_ [class_ "col-xs-12 col-md-10 offset-md-1"] $ do
                toHtml $ Feed True [("My Articles", "", True), ("Favorited Articles", "", False)] []
  toHtmlRaw = toHtml

type ProfileRoute = "profile" :> Auth '[Cookie] Model.User :> HXRequest :> Capture "username" Text :> Get '[HTML] (Partial Profile)

profileHandler :: AuthResult Model.User -> Maybe Text -> Text -> App (Partial Profile)
profileHandler (Authenticated currentUser) hxReq username = do
  result <- getUserByUsername username
  case result of
    Left _ -> throwError err401
    Right user -> do
      profile <-
        if Model.userID currentUser == Model.userID user
        then do
          pure $ ProfilePrivate currentUser
        else do
          -- See if currentUser is following the other
          isFollowingResult <- doesFollowExist currentUser user
          case isFollowingResult of
            Left _ -> throwError err401
            Right isFollowing ->
              pure $ ProfilePublic user isFollowing
      pure $ case hxReq of
        Just "true" -> NotWrapped profile
        _ -> Wrapped (Just currentUser) profile
profileHandler _ hxReq username = do
  result <- getUserByUsername username
  case result of
    Left _ -> throwError err401
    Right user -> pure $ case hxReq of
        Just "true" -> NotWrapped $ ProfilePublic user False
        _ -> Wrapped Nothing $ ProfilePublic user False
-- Profile End --

-- Settings Start --

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
              button_ [class_ "btn btn-outline-danger", hxPostSafe_ logoutLink, hxTarget_ "#content-slot"] $ "Or click here to logout."
  toHtmlRaw = toHtml

type SettingsRoute = "settings" :> Auth '[Cookie] Model.User :> HXRequest :> Get '[HTML] (Partial Settings)

settingsHandler :: AuthResult Model.User -> Maybe Text -> App (Partial Settings)
settingsHandler (Authenticated currentUser) hxReq = 
  pure $ case hxReq of
    Just "true" -> NotWrapped $ Settings currentUser
    _ -> Wrapped (Just currentUser) $ Settings currentUser
settingsHandler _ _ = redirectToRegisterForm

-- Settings End --

-- Editor Start --

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

type EditorRoute = "editor" :> Auth '[Cookie] Model.User :> HXRequest :> Get '[HTML] (Partial Editor)

editorHandler :: AuthResult Model.User -> Maybe Text -> App (Partial Editor)
editorHandler (Authenticated currentUser) hxReq =
  pure $ case hxReq of
    Just "true" -> NotWrapped Editor
    _ -> Wrapped (Just currentUser) Editor
editorHandler _ _ = redirectToRegisterForm

-- Editor End --

-- Article Start --

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
              "Follow Eric Simons"
              span_ [class_ "counter"] "(10)"
            button_ [class_ "btn btn-sm btn-outline-primary"] $ do
              i_ [class_ "ion-heart"] ""
              "Favorite Post"
              span_ [class_ "counter"] "(29)"
      div_ [class_ "container page"] $ do
        div_ [class_ "row article-content"] $
          div_ [class_ "col-md-12"] $ do
            p_ "Web development technologies have evolved at an incredible clip over the past few years."
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
              "Follow Eric Simons"
            button_ [class_ "btn btn-sm btn-outline-primary"] $ do
              i_ [class_ "ion-heart"] ""
              "Favorite Post"
              span_ [class_ "counter"] "(29)"
        div_ [class_ "row"] $
          div_ [class_ "col-xs-12 col-md-8 offset-md-2"] $ do
            form_ [class_ "card comment-form"] $ do
              div_ [class_ "card-block"] $ textarea_ [class_ "form-control", placeholder_ "Write a comment...", rows_ "3"] ""
              div_ [class_ "card-footer"] $ do
                img_ [src_ "http://i.imgur.com/Qr71crq.jpg", class_ "comment-author-img"]
                button_ [class_ "btn btn-sm btn-primary"] "Post Comment"
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

-- TODO: Implement Route and Handler

-- Article End --

-- Register Response Start --

data RegisterResponse
  = RegisterFailure Model.RegisterForm [Text]
  | RegisterSuccess Model.User

instance ToHtml RegisterResponse where
  toHtml (RegisterFailure registerForm errors) = toHtml $ RegisterForm (Just registerForm) errors
  toHtml (RegisterSuccess user) = do
    toHtml $ Home $ Just user
    toHtml $ Navbar (Just user) True
  toHtmlRaw = toHtml

type RegisterRoute =
  "register"
    :> ReqBody '[FormUrlEncoded] Model.RegisterForm
    :> Post '[HTML] (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] RegisterResponse)

registerHandler ::
  CookieSettings ->
  JWTSettings ->
  Model.RegisterForm ->
  App (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] RegisterResponse)
registerHandler
  cookieSettings
  jwtSettings
  registerForm = do
    -- Check that submission is valid
    formResult <- runForm Validate.registerForm (toJSON registerForm)
    case formResult of
      ParsingFailed _ parseErr ->
        [parseErr]
          & RegisterFailure registerForm
          & noHeader
          & noHeader
          & noHeader
          & pure
      ValidationFailed errMap ->
        errMap
          & toList
          & Prelude.map (\(field, f) -> f $ showFieldName field)
          & RegisterFailure registerForm
          & noHeader
          & noHeader
          & noHeader
          & pure
      Succeeded registerForm -> do
        -- If valid form, write user to DB and return creds
        -- TODO: Write user info to DB and getCreds
        result <- insertUser registerForm
        case result of
          Left err ->
            RegisterFailure registerForm [pack $ show err]
              & noHeader
              & noHeader
              & noHeader
              & pure
          Right user -> do
            mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
            case mApplyCookies of
              Nothing -> throwError err401
              Just applyCookies ->
                RegisterSuccess user
                  & applyCookies
                  & addHeader (toUrl homeLink)
                  & pure

-- Register Response End --

-- Login Response Start --

data LoginResponse
  = LoginFailure Model.LoginForm [Text]
  | LoginSuccess Model.User

instance ToHtml LoginResponse where
  toHtml (LoginFailure loginForm errors) = toHtml $ LoginForm (Just loginForm) errors
  toHtml (LoginSuccess user) = do
    toHtml $ Home $ Just user
    toHtml $ Navbar (Just user) True
  toHtmlRaw = toHtml

type LoginRoute =
  "login"
    :> ReqBody '[FormUrlEncoded] Model.LoginForm
    :> Post '[HTML] (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)

loginHandler ::
  CookieSettings ->
  JWTSettings ->
  Model.LoginForm ->
  App (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)
loginHandler
  cookieSettings
  jwtSettings
  loginForm = do
    -- Check that submission is valid
    formResult <- runForm Validate.loginForm (toJSON loginForm)
    liftIO $ print loginForm
    case formResult of
      ParsingFailed _ parseErr ->
        [parseErr]
          & LoginFailure loginForm
          & noHeader
          & noHeader
          & noHeader
          & pure
      ValidationFailed errMap ->
        errMap
          & toList
          & Prelude.map (\(field, f) -> f $ showFieldName field)
          & LoginFailure loginForm
          & noHeader
          & noHeader
          & noHeader
          & pure
      Succeeded loginForm -> do
        -- If valid form, fetch user from DB and return creds
        -- TODO: Fetch user info from DB and getCreds
        result <- verifyUser loginForm
        case result of
          Left err ->
            LoginFailure loginForm [pack . show $ err]
              & noHeader
              & noHeader
              & noHeader
              & pure
          Right user -> do
            mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
            case mApplyCookies of
              Nothing -> throwError err401
              Just applyCookies ->
                applyCookies (LoginSuccess user)
                  & addHeader (toUrl homeLink)
                  & pure

-- Login Response End --

-- Logout Response Start --

data LogoutResponse = LogoutResponse

instance ToHtml LogoutResponse where
  toHtml LogoutResponse = do
    toHtml $ Home Nothing
    toHtml $ Navbar Nothing True
  toHtmlRaw = toHtml

type LogoutRoute =
  "logout"
  :> Auth '[Cookie] Model.User
  :> Post '[HTML] (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LogoutResponse)

logoutHandler :: AuthResult Model.User -> App (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LogoutResponse)
logoutHandler _ = do
  LogoutResponse
  & noHeader
  & noHeader
  & addHeader (toUrl homeLink)
  & pure

-- Logout Response End --

-- Follow Button Start --

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
        " Follow " <> toHtml followee <> " "
  toHtmlRaw = toHtml

type FollowRoute = "follow" :> Auth '[Cookie] Model.User :> ReqBody '[FormUrlEncoded] Model.FollowForm :> Post '[HTML] UnfollowButton

followHandler :: AuthResult Model.User -> Model.FollowForm -> App UnfollowButton
followHandler = undefined

-- Follow Button End --

-- Unfollow Button Start --

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
        " Unfollow " <> toHtml unfollowee <> " "
  toHtmlRaw = toHtml

type UnfollowRoute = "unfollow" :> Auth '[Cookie] Model.User :> ReqBody '[FormUrlEncoded] Model.UnfollowForm :> Post '[HTML] FollowButton

unfollowHandler :: AuthResult Model.User -> Model.UnfollowForm -> App FollowButton
unfollowHandler = undefined

-- Unfollow Button End --

type Routes =
    HomeRoute
    :<|> FollowRoute
    :<|> UnfollowRoute
    :<|> EditorRoute
    :<|> SettingsRoute
    :<|> ProfileRoute
    :<|> LogoutRoute
    :<|> RegisterFormRoute
    :<|> LoginFormRoute
    :<|> RegisterRoute
    :<|> LoginRoute

loginFormLink :: Link
loginFormLink = getLink $ proxy @LoginFormRoute

loginFormSubmitLink :: Link
loginFormSubmitLink = getLink $ proxy @LoginRoute

registerFormLink :: Link
registerFormLink = getLink $ proxy @RegisterFormRoute

registerFormSubmitLink :: Link
registerFormSubmitLink = getLink $ proxy @RegisterRoute

homeLink :: Link
homeLink = getLink $ proxy @HomeRoute

profileLink :: Text -> Link
profileLink = getLink $ proxy @ProfileRoute

followLink :: Link
followLink = getLink $ proxy @FollowRoute

unfollowLink :: Link
unfollowLink = getLink $ proxy @UnfollowRoute

editorLink :: Link
editorLink = getLink $ proxy @EditorRoute

settingsLink :: Link
settingsLink = getLink $ proxy @SettingsRoute

logoutLink :: Link
logoutLink = getLink $ proxy @LogoutRoute
