{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CSDC.SQL.Forum
  ( insertThread,
    selectThreads,
    insertPost,
    selectPosts,
  )
where

import CSDC.Prelude
import CSDC.SQL.Decoder qualified as Decoder
import CSDC.SQL.Encoder qualified as Encoder
import CSDC.SQL.QQ
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

insertThread :: Statement Thread (Id Thread)
insertThread = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      INSERT INTO threads
        (unit, author, subject)
      VALUES
        ($1,$2,$3)
      RETURNING
        id
      |]

    encoder =
      contramap (.unitId) Encoder.id
        <> contramap (.authorId) Encoder.id
        <> contramap (.subject) Encoder.text

    decoder = Decoder.singleRow Decoder.id

selectThreads :: Statement (Id Unit) [ThreadInfo]
selectThreads = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
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
      id <- Decoder.id
      unitId <- Decoder.id
      authorId <- Decoder.id
      authorName <- Decoder.text
      subject <- Decoder.text
      createdAt <- Decoder.posixTime
      last <- Decoder.posixTime
      messages <- Decoder.int
      pure ThreadInfo {..}

insertPost :: Statement Post (Id Post)
insertPost = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      INSERT INTO posts
        (thread, author, text)
      VALUES
        ($1,$2,$3)
      RETURNING
        id
      |]

    encoder =
      contramap (.threadId) Encoder.id
        <> contramap (.authorId) Encoder.id
        <> contramap (.text) Encoder.text

    decoder = Decoder.singleRow Decoder.id

selectPosts :: Statement (Id Thread) [PostInfo]
selectPosts = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      SELECT
        posts.id, author, persons.name, persons.image, text, posts.created_at
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
      id <- Decoder.id
      authorId <- Decoder.id
      authorName <- Decoder.text
      authorImage <- Decoder.text
      text <- Decoder.text
      createdAt <- Decoder.posixTime
      pure PostInfo {..}
