{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Conduit.Database where

import Conduit.App
import Conduit.Model
import Conduit.Model (RegisterForm (RegisterForm))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap)
import Data.Either (rights)
import Data.Function ((&))
import Data.Int
-- import qualified Data.Sequence.Internal.Sorting as Text
import Data.Profunctor (dimap)
import Data.Text
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Hasql.Connection (Connection)
import Hasql.Session (Session, QueryError)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement
import Hasql.TH (resultlessStatement)
import qualified Hasql.TH as TH

eitherToMaybe (Right a) = Just a
eitherToMaybe _ = Nothing

-- HELPERS START --

tupleToUser :: (Int32, Text, Text, Text, Text) -> User
tupleToUser (pk, username, email, imageUrl, bio) =
  User
    { userBio = bio,
      userEmail = email,
      userID = ID pk,
      userImageUrl = imageUrl,
      userUsername = username
    }

settingsFormToTuple :: SettingsForm -> (Int32, Text, Text, Text, Text, Text)
settingsFormToTuple SettingsForm {..} =
  ( unID settingsFormUserID,
    settingsFormUsername,
    settingsFormEmail,
    settingsFormImageUrl,
    settingsFormBio,
    settingsFormNewPassword
  )

registerFormToTuple :: RegisterForm -> (Text, Text, Text)
registerFormToTuple RegisterForm {..} =
  ( registerFormUsername,
    registerFormEmail,
    registerFormPassword
  )

publishFormWithAuthorIDToTuple :: (ID User, PublishForm) -> (Int32, Text, Text, Text)
publishFormWithAuthorIDToTuple (authorID, PublishForm {..}) =
  (unID authorID, publishFormTitle, publishFormDescription, publishFormBody)

tupleToArticle :: (Int32, Int32, Text, Text, Text, Int32, UTCTime) -> Article
tupleToArticle (articleID, authorID, title, description, body, favorites, createdAt) =
  Article
    { articleAuthorID = ID articleID,
      articleBody = body,
      articleCreatedAt = createdAt,
      articleDescription = description,
      articleFavorites = favorites,
      articleID = ID articleID,
      articleTitle = title
    }

editArticleFormWithAuthorIDToTuple :: (ID User, EditArticleForm) -> (Int32, Int32, Text, Text, Text)
editArticleFormWithAuthorIDToTuple (authorID, EditArticleForm {..}) =
  (unID authorID, unID editArticleFormArticleID, editArticleFormTitle, editArticleFormDescription, editArticleFormBody)

commentFormToTuple :: CommentForm -> (Int32, Int32, Text)
commentFormToTuple CommentForm {..} =
  (unID commentFormAuthorID, unID commentFormArticleID, commentFormBody)

tupleToComment :: (Int32, Int32, Int32, Text, UTCTime) -> Comment
tupleToComment (commentID, authorID, articleID, body, createdAt) =
  Comment
    { commentArticleID = ID articleID,
      commentAuthorID = ID authorID,
      commentBody = body,
      commentCreatedAt = createdAt,
      commentID = ID commentID
    }

tupleToArticleTagsVectors :: (ID Article, Text) -> (Vector Int32, Vector Text)
tupleToArticleTagsVectors (ID articleID, commaSepTags) =
  [ (articleID, tag)
    | tag <- Prelude.map strip $ split (',' ==) commaSepTags
  ]
    & Prelude.unzip
    & bimap Vector.fromList Vector.fromList

followToTuple :: Follow -> (Int32, Int32)
followToTuple ((ID followerID) :-> (ID followeeID)) =
  (followerID, followeeID)

tupleToFollow :: (Int32, Int32) -> Follow
tupleToFollow (followerID, followeeID) = ID followerID :-> ID followeeID

-- HELPERS END --

-- USERS START --

dropUsersSession :: Session ()
dropUsersSession =
  Session.sql
    [TH.uncheckedSql| drop table if exists users cascade |]

createUsersSession :: Session ()
createUsersSession =
  Session.sql
    [TH.uncheckedSql|
      create table if not exists users (
        pk_user serial primary key,
        username text unique not null,
        email text unique not null,
        imageUrl text not null default 'https://api.realworld.io/images/smiley-cyrus.jpeg',
        bio text not null default '',
        hash text not null
      );
    |]

insertUserStatement :: Statement RegisterForm User
insertUserStatement =
  dimap
    registerFormToTuple
    tupleToUser
    [TH.singletonStatement|
      insert into users (username, email, hash)
      values ($1 :: text, $2 :: text, crypt($3 :: text, gen_salt('bf')))
      returning pk_user :: int4, username :: text, email :: text, imageUrl :: text, bio :: text
    |]

updateUserStatement :: Statement SettingsForm User
updateUserStatement =
  dimap
    settingsFormToTuple
    tupleToUser
    [TH.singletonStatement|
      update users
      set username = $2 :: Text,
        email = $3 :: Text,
        imageUrl = $4 :: Text,
        bio = $5 :: Text,
        hash =
          case
            when len($6 :: text) = 0 then hash
            else crypt($6 :: text, gen_salt('bf'))
          end
      where pk_user = $1 :: int4
      returning pk_user :: int4, username :: text, email :: text, imageUrl :: text, bio :: text
    |]

verifyUserStatement :: Statement LoginForm User
verifyUserStatement =
  dimap
    loginFormToTuple
    tupleToUser
    [TH.singletonStatement|
      select
        pk_user :: int4,
        username :: text,
        email :: text,
        imageUrl :: text,
        bio :: text
      from users
      where email = $1 :: text and hash = crypt($2 :: text, hash)
    |]
  where
    loginFormToTuple :: LoginForm -> (Text, Text)
    loginFormToTuple LoginForm{..} = (loginFormEmail, loginFormPassword)

-- USERS END --

-- ARTICLES START --

dropArticlesSession :: Session ()
dropArticlesSession =
  Session.sql
    [TH.uncheckedSql| drop table if exists articles cascade |]

createArticlesSession :: Session ()
createArticlesSession =
  Session.sql
    [TH.uncheckedSql|
      create table if not exists articles (
        pk_article serial primary key,
        fk_user serial references users(pk_user) on delete cascade,
        title text unique not null,
        description text not null default '',
        body text not null default '',
        favorites int4 not null default 0,
        created_at timestamptz not null default now()
      );
    |]

insertArticleStatement :: Statement (ID User, PublishForm) Article
insertArticleStatement =
  dimap
    publishFormWithAuthorIDToTuple
    tupleToArticle
    [TH.singletonStatement|
      insert into articles (fk_user, title, description, body)
      values ($1 :: int4, $2 :: text, $3 :: text, $4 :: text)
      returning pk_article :: int4, fk_user :: int4, title :: text, description :: text, body :: text, favorites :: int4, created_at :: timestamptz
    |]

updateArticleStatement :: Statement (ID User, EditArticleForm) Article
updateArticleStatement =
  dimap
    editArticleFormWithAuthorIDToTuple
    tupleToArticle
    [TH.singletonStatement|
        update articles
        set title = $3 :: Text,
            description = $4 :: Text,
            body = $5 :: Text
        where fk_user = $1 :: int4 and pk_article = $2 :: int4
        returning pk_article :: int4, fk_user :: int4, title :: text, description :: text, body :: text, favorites :: int4, created_at :: timestamptz
    |]

deleteArticleStatement :: Statement (ID Article) Int64
deleteArticleStatement =
  dimap
    unID
    id
    [TH.rowsAffectedStatement|
      delete from articles where pk_article = $1 :: int4
    |]

-- ARTICLES END --

-- COMMENTS START --

dropCommentsSession :: Session ()
dropCommentsSession =
  Session.sql
    [TH.uncheckedSql| drop table if exists comments cascade |]

createCommentsSession :: Session ()
createCommentsSession =
  Session.sql
    [TH.uncheckedSql|
      create table if not exists comments (
        pk_comment serial primary key,
        fk_user serial references users(pk_user) on delete cascade,
        fk_article serial references articles(pk_article) on delete cascade,
        body text not null default '',
        created_at timestamptz not null default now()
      );
    |]

insertCommentStatement :: Statement CommentForm Comment
insertCommentStatement =
  dimap
    commentFormToTuple
    tupleToComment
    [TH.singletonStatement|
      insert into articles (fk_user, fk_article, body)
      values ($1 :: int4, $2 :: int4, $3 :: text)
      returning pk_comment :: int4, fk_user :: int4, fk_article :: int4, body :: text, created_at :: timestamptz
    |]

deleteCommentStatement :: Statement (ID Comment) Int64
deleteCommentStatement =
  dimap
    unID
    id
    [TH.rowsAffectedStatement|
      delete from comments where pk_comment = $1 :: int4
    |]

-- COMMENTS END --

-- TAGS START --

-- dropTagsSession :: Session ()
-- dropTagsSession =
--   Session.sql
--     [TH.uncheckedSql| drop table if exists tags |]

-- createTagsSession :: Session ()
-- createTagsSession =
--   Session.sql
--     [TH.uncheckedSql|
--         create table if not exists tags (
--             pk_tag text primary key
--         );
--     |]

-- insertTagsStatement :: Statement Text ()
-- insertTagsStatement =
--   dimap
--     (fromList . Prelude.map strip . split (',' ==))
--     id
--     [TH.resultlessStatement|
--       insert into tags (pk_tag)
--       select * from unnest ($1 :: text[])
--     |]

-- TAGS END --

-- ARTICLES TAGS START --

dropArticlesTagsSession :: Session ()
dropArticlesTagsSession =
  Session.sql
    [TH.uncheckedSql| drop table if exists articles_tags |]

createArticlesTagsSession :: Session ()
createArticlesTagsSession =
  Session.sql
    [TH.uncheckedSql|
      create table if not exists articles_tags (
        fk_article serial references articles(pk_article) on delete cascade,
        tag text,
        constraint pk_articles_tags primary key (fk_article, tag)
      );
    |]

insertArticleTagsStatement :: Statement (ID Article, Text) ()
insertArticleTagsStatement =
  dimap
    tupleToArticleTagsVectors
    id
    [TH.resultlessStatement|
      insert into articles_tags (fk_article, tag)
      select * from unnest ($1 :: int4[], $2 :: text[])
    |]

-- ARTICLES TAGS END --

-- FOLLOWS START --

dropFollowsSession :: Session ()
dropFollowsSession =
  Session.sql
    [TH.uncheckedSql| drop table if exists follows |]

createFollowsSession :: Session ()
createFollowsSession =
  Session.sql
    [TH.uncheckedSql|
      create table if not exists follows (
        fk_follower serial references users(pk_user) on delete cascade,
        fk_followee serial references users(pk_user) on delete cascade,
        constraint pk_follow primary key (fk_follower, fk_followee)
      );
    |]

insertFollowStatement :: Statement Follow Follow
insertFollowStatement =
  dimap
    followToTuple
    tupleToFollow
    [TH.singletonStatement|
      insert into follows (fk_follower, fk_followee)
      values ($1 :: int4, $2 :: int4)
      returning fk_follower :: int4, fk_followee :: int4
    |]

-- FOLLOWS END --

getUserByUsernameStatement :: Statement Text User
getUserByUsernameStatement =
  dimap
    id
    tupleToUser
    [TH.singletonStatement|
      select
        pk_user :: int4,
        username :: text,
        email :: text,
        imageUrl :: text,
        bio :: text
      from users
      where username = $1 :: text
    |]

doesFollowExistStatment :: Statement (Int32, Int32) Bool
doesFollowExistStatment =
  dimap
    id
    id
    [TH.singletonStatement|
      select exists (select * from follows where fk_follower = $1 :: int4 and fk_followee = $2 :: int4) :: bool
    |]

getTagsByArticleIDStatement :: Statement (ID Article) [Tag]
getTagsByArticleIDStatement =
  dimap
    unID
    (Vector.toList . fmap Tag)
    [TH.vectorStatement|
      select
        fk_tag :: text
      from article_tags at
      where at.fk_article = $1 :: int4
    |]

getUserByUserIDStatement :: Statement (ID User) User
getUserByUserIDStatement =
  dimap
    unID
    tupleToUser
    [TH.singletonStatement|
      select
        pk_user :: int4,
        username :: text,
        email :: text,
        imageUrl :: text,
        bio :: text
      from users
      where pk_user = $1 :: int4
    |]

getAllArticlesStatement :: Statement () [Article]
getAllArticlesStatement =
  dimap
    id
    (Vector.toList . fmap tupleToArticle)
    [TH.vectorStatement|
      select
        pk_article :: int4,
        fk_user :: int4,
        title :: text,
        description :: text,
        body :: text,
        favorites :: int4,
        created_at :: timestamptz
      from articles
      order by "created_at"
    |]

getArticlesByTagStatement :: Statement Tag [Article]
getArticlesByTagStatement =
  dimap
    unTag
    (Vector.toList . fmap tupleToArticle)
    [TH.vectorStatement|
      select
        pk_article :: int4,
        fk_user :: int4,
        title :: text,
        description :: text,
        body :: text,
        favorites :: int4,
        created_at :: timestamptz
      from articles
      inner join article_tags on fk_tag = $1 :: text
      where pk_article = fk_article
      order by "created_at"
    |]

getArticlesByUserIDStatement :: Statement (ID User) [Article]
getArticlesByUserIDStatement =
  dimap
    unID
    (Vector.toList . fmap tupleToArticle)
    [TH.vectorStatement|
      select
        pk_article :: int4,
        fk_user :: int4,
        title :: text,
        description :: text,
        body :: text,
        favorites :: int4,
        created_at :: timestamptz
      from articles
      where fk_user = $1 :: int4
      order by "created_at"
    |]

getArticleByArticleIDStatement :: Statement (ID Article) Article
getArticleByArticleIDStatement =
  dimap
    unID
    tupleToArticle
    [TH.singletonStatement|
      select
        pk_article :: int4,
        fk_user :: int4,
        title :: text,
        description :: text,
        body :: text,
        favorites :: int4,
        created_at :: timestamptz
      from articles
      where pk_article = $1 :: int4
    |]

-- EXECUTION HELPERS START --

runStatementIO :: forall i o. Connection -> Statement i o -> i -> IO (Either QueryError o)
runStatementIO conn statement input = Session.run (Session.statement input statement) conn

runUncheckedSqlIO :: Connection -> Session () -> IO (Either QueryError ())
runUncheckedSqlIO conn session = Session.run session conn

runStatement :: forall i o. Statement i o -> i -> App (Either QueryError o)
runStatement statement input = do
  conn <- grab @Connection
  liftIO $ runStatementIO conn statement input

runUncheckedSql :: Session () -> App (Either QueryError ())
runUncheckedSql session = do
  conn <- grab @Connection
  liftIO $ runUncheckedSqlIO conn session

-- EXECUTION HELPERS END --

-- EXECUTION START --

insertUser :: RegisterForm -> App (Either QueryError User)
insertUser = runStatement insertUserStatement

insertArticle :: (ID User, PublishForm) -> App (Either QueryError Article)
insertArticle = runStatement insertArticleStatement

updateArticle :: (ID User, EditArticleForm) -> App (Either QueryError Article)
updateArticle = runStatement updateArticleStatement

verifyUser :: LoginForm -> App (Either QueryError User)
verifyUser = runStatement verifyUserStatement

getUserByUsername :: Text -> App (Either QueryError User)
getUserByUsername = runStatement getUserByUsernameStatement

doesFollowExist :: User -> User -> App (Either QueryError Bool)
doesFollowExist user1 user2 = runStatement doesFollowExistStatment (unID $ userID user1, unID $ userID user2)

getAllArticles :: App (Either QueryError [Article])
getAllArticles = runStatement getAllArticlesStatement ()

getArticlesByTag :: Tag -> App (Either QueryError [Article])
getArticlesByTag = runStatement getArticlesByTagStatement

getArticlesByUserID :: ID User -> App (Either QueryError [Article])
getArticlesByUserID = runStatement getArticlesByUserIDStatement

getArticleByArticleID :: ID Article -> App (Either QueryError Article)
getArticleByArticleID = runStatement getArticleByArticleIDStatement

getArticleInfo :: Article -> App (Maybe ArticleInfo)
getArticleInfo article@Article{..} = eitherToMaybe <$> do
  authorResult <- runStatement getUserByUserIDStatement articleAuthorID
  case authorResult of
    Left err' -> pure $ Left err'
    Right author -> do
      tagsResult <- runStatement getTagsByArticleIDStatement articleID
      case tagsResult of
        Left err'' -> pure $ Left err''
        Right tags -> pure $ Right (article, author, tags)

getArticleInfos :: [Article] -> App [ArticleInfo]
getArticleInfos articles = rights <$> do
  forM articles $ \article@Article{..} -> do
    authorResult <- runStatement getUserByUserIDStatement articleAuthorID
    case authorResult of
      Left err' -> pure $ Left err'
      Right author -> do
        tagsResult <- runStatement getTagsByArticleIDStatement articleID
        case tagsResult of
          Left err'' -> pure $ Left err''
          Right tags -> pure $ Right (article, author, tags)

-- EXECUTION END --

{-
insertCfd :: Connection.Connection -> ContactFormData -> IO (Either QueryError Contact)
insertCfd conn cfd = Session.run (Session.statement cfd insertCfdStatement) conn

insertCfds :: Connection.Connection -> [ContactFormData] -> IO (Either QueryError ())
insertCfds conn cfds = Session.run (Session.statement cfds insertCfdsStatement) conn

selectContact :: Connection.Connection -> ID Contact -> IO (Either QueryError Contact)
selectContact conn cID = Session.run (Session.statement cID selectContactStatement) conn

selectContactTable :: Connection.Connection -> IO (Either QueryError ContactTable)
selectContactTable conn = Session.run (Session.statement () selectContactTableStatement) conn

deleteContact :: Connection.Connection -> ID Contact -> IO (Either QueryError ())
deleteContact conn cID = Session.run (Session.statement cID deleteContactStatement) conn

updateContact :: Connection.Connection -> (ID Contact, ContactFormData) -> IO (Either QueryError Contact)
updateContact conn cfdWithID = Session.run (Session.statement cfdWithID updateContactStatement) conn
-}
