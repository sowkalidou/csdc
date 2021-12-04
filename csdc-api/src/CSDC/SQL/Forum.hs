{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module CSDC.SQL.Forum
  ( insertThread
  , selectThreads
  , insertPost
  , selectPosts
  ) where

import CSDC.Prelude
import CSDC.SQL.QQ

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

insertThread :: Statement Thread (Id Thread)
insertThread = Statement sql encoder decoder True
  where
    sql = [sqlqq|
      INSERT INTO threads
        (unit, author, subject)
      VALUES
        ($1,$2,$3)
      RETURNING
        id
      |]

    encoder =
      contramap thread_unit Encoder.id <>
      contramap thread_author Encoder.id <>
      contramap thread_subject Encoder.text

    decoder = Decoder.singleRow Decoder.id

selectThreads :: Statement (Id Unit) [ThreadInfo]
selectThreads = Statement sql encoder decoder True
  where
    sql = [sqlqq|
      SELECT
        threads.id, unit, threads.author, persons.name, subject,
        threads.created_at, MAX(posts.created_at) AS last, COUNT(posts.id) AS number
      FROM
        threads
      JOIN
        posts ON posts.thread = threads.id
      JOIN
        persons ON persons.id = threads.author
      WHERE
        unit = $1
      GROUP BY
        threads.id, persons.name
      ORDER BY
        last DESC
      |]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      threadInfo_id <- Decoder.id
      threadInfo_unit <- Decoder.id
      threadInfo_author <- Decoder.id
      threadInfo_authorName <- Decoder.text
      threadInfo_subject <- Decoder.text
      threadInfo_createdAt <- Decoder.timestamptz
      threadInfo_last <- Decoder.timestamptz
      threadInfo_messages <- Decoder.int
      pure ThreadInfo {..}

insertPost :: Statement Post (Id Post)
insertPost = Statement sql encoder decoder True
  where
    sql = [sqlqq|
      INSERT INTO threads
        (thread, author, text)
      VALUES
        ($1,$2,$3)
      RETURNING
        id
      |]

    encoder =
      contramap post_thread Encoder.id <>
      contramap post_author Encoder.id <>
      contramap post_text Encoder.text

    decoder = Decoder.singleRow Decoder.id

selectPosts :: Statement (Id Thread) [PostInfo]
selectPosts = Statement sql encoder decoder True
  where
    sql = [sqlqq|
      SELECT
        posts.id, author, persons.name, text, posts.created_at
      FROM
        posts
      JOIN
        persons ON persons.id = posts.author
      WHERE
        thread = $1
      ORDER BY
        created_at ASC
      |]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      postInfo_id <- Decoder.id
      postInfo_author <- Decoder.id
      postInfo_authorName <- Decoder.text
      postInfo_text <- Decoder.text
      postInfo_createdAt <- Decoder.timestamptz
      pure PostInfo {..}

