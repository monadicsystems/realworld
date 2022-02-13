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
        created_at timestampz not null default now()
      );
    |]

insertArticle :: Statement NewEditorForm Article
insertArticle =
  dimap
    editorFormToTuple
    tupleToArticle
    [TH.singletonStatement|
            insert into articles (title, description, body)
            values ($1 :: text, $2 :: text, $3 :: text)
            returning pk_article :: int4, title :: text, description :: text, body :: text, favorites :: int4
        |]
  where
    editorFormToTuple :: NewEditorForm -> (Text, Text, Text)
    editorFormToTuple NewEditorForm {..} =
      (editorTitle, editorFormDescription, editorBody)

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
        returning returning pk_article :: int4, title :: text, description :: text, body :: text, favorites :: int4
    |]
  where
    editorFormToTuple :: UpdateEditorForm -> (Int32, Text, Text, Text)
    editorFormToTuple EditorForm {..} =
      (unID editorFormArticleID, editorTitle, editorFormDescription, editorBody)

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
        created_at timestampz not null default now()
      );
    |]

insertComment :: Statement CommentForm Comment
insertComment =
  dimap
    editorFormToTuple
    tupleToArticle
    [TH.singletonStatement|
            insert into articles (title, description, body)
            values ($1 :: text, $2 :: text, $3 :: text)
            returning pk_article :: int4, title :: text, description :: text, body :: text, favorites :: int4
        |]
  where
    editorFormToTuple :: NewEditorForm -> (Text, Text, Text)
    editorFormToTuple NewEditorForm {..} =
      (editorTitle, editorFormDescription, editorBody)

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
    ((split (',' ==) & map strip & fromList))
    id
    [TH.resultlessStatement|
      insert into tags (pk_tag)
      select * from unnest ($1 :: text[])
    |]

-- TAGS END --

-- ARTICLES TAGS END --

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

editorFormToArticlesTagsVectors :: EditorForm -> (Vector Int32, Vector Text)
editorFormToArticlesTagsVectors EditorForm {..} =
  [ (unID editorFormArticleID, tag)
    | tag <- map strip $ split (',' ==) editorFormTags
  ]
    & unzip
    & dimap fromList fromList

insertArticlesTags :: Statement EditorForm ()
insertArticlesTags =
  dimap
    editorFormToArticleTagsVectors
    id
    [TH.resultlessStatement|
      insert into articles_tags (fk_article, fk_tag)
      select * from unnest ($1 :: int4[], $2 :: text[])
    |]

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