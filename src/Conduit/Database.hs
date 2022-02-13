{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Conduit.Database where

-- from "hasql-th"

import Conduit.Model
import Conduit.Model (SignUpForm (SignUpForm))
import Data.Int
import qualified Data.Sequence.Internal.Sorting as Text
import Data.Text
import Data.Time
import Data.Vector (Vector)
import Data.Vector as Vector
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement
import Hasql.TH (resultlessStatement)
import qualified Hasql.TH as TH

-- USERS START --

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
    settingsFormUserUsername,
    settingsFormUserEmail,
    settingsFormImageUrl,
    settingsFormBio,
    settingsFormNewPassword
  )

signUpFormToTuple :: SignUpForm -> (Text, Text, Text)
signUpFormToTuple SignUpForm {..} =
  ( signUpFormUsername,
    signUpFormEmail,
    signUpFormPassword
  )

dropUsersTable :: Session ()
dropUsersTable =
  Session.sql
    [TH.uncheckedSql| drop table if exists users |]

createUsersTable :: Session ()
createUsersTable =
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

insertUser :: Statement SignUpForm User
insertUser =
  dimap
    signUpFormToTuple
    tupleToUser
    [TH.singletonStatement|
      insert into users (username, email, hash)
      values ($1 :: text, $2 :: text, crypt($4 :: text, gen_salt('bf')))
      returning pk_user :: int4, username :: text, email :: text, imageUrl :: text, bio :: text
    |]

updateUser :: Statement SettingsForm User
updateUser =
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

-- USERS END --

-- ARTICLES START --

dropArticlesTable :: Session ()
dropArticlesTable =
  Session.sql
    [TH.uncheckedSql| drop table if exists articles |]

createArticlesTable :: Session ()
createArticlesTable =
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

newEditorFormToTuple :: NewEditorForm -> (Int32, Text, Text, Text)
newEditorFormToTuple NewEditorForm {..} =
  (unID newEditorFormAuthorID, newEditorFormTitle, newEditorFormDescription, newEditorFormBody)

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

insertArticle :: Statement NewEditorForm Article
insertArticle =
  dimap
    newEditorFormToTuple
    tupleToArticle
    [TH.singletonStatement|
      insert into articles (title, description, body)
      values ($1 :: text, $2 :: text, $3 :: text)
      returning pk_article :: int4, fk_user :: int4, title :: text, description :: text, body :: text, favorites :: int4, created_at :: timestamptz
    |]

updateEditorFormToTuple :: UpdateEditorForm -> (Int32, Text, Text, Text)
updateEditorFormToTuple UpdateEditorForm {..} =
  (unID updateEditorFormArticleID, updateEditorFormTitle, updateEditorFormDescription, updateEditorFormBody)

updateArticle :: Statement UpdateEditorForm Article
updateArticle =
  dimap
    editorFormToTuple
    tupleToArticle
    [TH.singletonStatement|
        update articles
        set title = $2 :: Text,
            description = $3 :: Text,
            body = $4 :: Text,
        where pk_article = $1 :: int4
        returning pk_article :: int4, fk_user :: int4, title :: text, description :: text, body :: text, favorites :: int4, created_at :: timestamptz
    |]

deleteArticle :: Statement (ID Article) Int64
deleteArticle =
  dimap
    unID
    id
    [TH.rowsAffectedStatment|
      delete from articles where pk_article = $1 :: int4
    |]

-- ARTICLES END --

-- COMMENTS START --

dropCommentsTable :: Session ()
dropCommentsTable =
  Session.sql
    [TH.uncheckedSql| drop table if exists comments |]

createCommentsTable :: Session ()
createCommentsTable =
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

insertComment :: Statement CommentForm Comment
insertComment =
  dimap
    commentFormToTuple
    tupleToComment
    [TH.singletonStatement|
      insert into articles (fk_user, fk_article, body)
      values ($1 :: int4, $2 :: int4, $3 :: text)
      returning pk_comment :: int4, fk_user :: int4, fk_article :: int4, body :: text, created_at :: timestamptz
    |]

deleteComment :: Statement (ID Comment) Int64
deleteComment =
  dimap
    unID
    id
    [TH.rowsAffectedStatment|
      delete from comments where pk_comment = $1 :: int4
    |]

-- COMMENTS END --

-- TAGS START --

dropTagsTable :: Session ()
dropTagsTable =
  Session.sql
    [TH.uncheckedSql| drop table if exists tags |]

createTagsTable :: Session ()
createTagsTable =
  Session.sql
    [TH.uncheckedSql|
        create table if not exists tags (
            pk_tag text primary key
        );
    |]

insertTags :: Statement Text ()
insertTags =
  dimap
    (split (',' ==) & map strip & fromList)
    id
    [TH.resultlessStatement|
      insert into tags (pk_tag)
      select * from unnest ($1 :: text[])
    |]

-- TAGS END --

-- ARTICLES TAGS START --

dropArticlesTagsTable :: Session ()
dropArticlesTagsTable =
  Session.sql
    [TH.uncheckedSql| drop table if exists articles_tags |]

createArticlesTagsTable :: Session ()
createArticlesTagsTable =
  Session.sql
    [TH.uncheckedSql|
      create table if not exists articles_tags (
        fk_article serial references articles(pk_article) on delete cascade,
        fk_tag text references tags(pk_tag) on delete cascade,
        constraint pk_articles_tags primary key (fk_article, fk_tag)
      );
    |]

tupleToArticleTagsVectors :: (ID Article, Text) -> (Vector Int32, Vector Text)
tupleToArticleTagsVectors (ID articleID, commaSepTags) =
  [ (articleID, tag)
    | tag <- map strip $ split (',' ==) commaSepTags
  ]
    & unzip
    & dimap fromList fromList

insertArticleTags :: Statement (ID Article, Text) ()
insertArticleTags =
  dimap
    tupleToArticleTagsVectors
    id
    [TH.resultlessStatement|
      insert into articles_tags (fk_article, fk_tag)
      select * from unnest ($1 :: int4[], $2 :: text[])
    |]

-- ARTICLES TAGS END --

-- FOLLOWS START --

dropFollowsTable :: Session ()
dropFollowsTable =
  Session.sql
    [TH.uncheckedSql| drop table if exists follows |]

createFollowsTable :: Session ()
createFollowsTable =
  Session.sql
    [TH.uncheckedSql|
      create table if not exists follows (
        fk_follower serial references users(pk_user) on delete cascade,
        fk_followee serial references users(pk_user) on delete cascade,
        constraint pk_follow primary key (fk_follower, fk_followee)
      );
    |]

followToTuple :: Follow -> (Int32, Int32)
followToTuple (ID followerID, ID followeeID) =
  (followerID, followeeID) 

tupleToFollow :: (Int32, Int32) -> Follow
tupleToFollow (followerID, followeeID) = (ID followerID) :-> (ID followeeID)

insertArticlesTags :: Statement Follow Follow
insertArticlesTags =
  dimap
    followToTuple
    tupleToFollow
    [TH.resultlessStatement|
      insert into follows (fk_follower, fk_followee)
      values ($1 :: int4, $2 :: int4)
      returning fk_follower :: int4, fk_followee :: int4
    |]

-- FOLLOWS END --

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
