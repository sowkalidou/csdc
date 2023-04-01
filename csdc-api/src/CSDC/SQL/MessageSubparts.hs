{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CSDC.SQL.MessageSubparts
  ( sendMessage,
    updateMessage,
    selectSubpart,
    sendReply,
    viewReply,
    selectMessagesForUnit,
    selectRepliesForUnit,
    getUnitsForMessage,
  )
where

import CSDC.Prelude
import CSDC.SQL.Decoder qualified as Decoder
import CSDC.SQL.Encoder qualified as Encoder
import CSDC.SQL.QQ
import Data.ByteString.Char8 qualified as ByteString
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

viewReply :: Statement (Id (Reply NewSubpart)) ()
viewReply = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "UPDATE replies_subpart",
          "SET status = 'Seen' :: reply_status",
          "WHERE id = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.noResult

selectSubpart :: Statement (Id (Message NewSubpart)) (Maybe NewSubpart)
selectSubpart = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT child, parent",
          "FROM messages_subpart",
          "WHERE id = $1"
        ]

    encoder = Encoder.id

    decoder =
      Decoder.rowMaybe $
        NewSubpart
          <$> Decoder.id
          <*> Decoder.id

sendMessage :: Statement (NewMessage NewSubpart) (Id (Message NewSubpart))
sendMessage = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO messages_subpart (type, message, child, parent)",
          "VALUES ($1 :: message_type, $2, $3, $4)",
          "RETURNING id"
        ]

    encoder =
      (contramap (.messageType) Encoder.messageType)
        <> (contramap (.text) Encoder.text)
        <> (contramap (.value.childId) Encoder.id)
        <> (contramap (.value.parentId) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

updateMessage :: Statement (Id (Message NewSubpart), MessageStatus) ()
updateMessage = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "UPDATE messages_subpart",
          "SET status = $2 :: message_status",
          "WHERE id = $1"
        ]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.messageStatus

    decoder = Decoder.noResult

sendReply :: Statement (NewReply NewSubpart) (Id (Reply NewSubpart))
sendReply = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO replies_subpart (type, reply, message)",
          "VALUES ($1 :: reply_type, $2, $3)",
          "RETURNING id"
        ]

    encoder =
      (contramap (.replyType) Encoder.replyType)
        <> (contramap (.text) Encoder.text)
        <> (contramap (.messageId) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

selectMessagesForUnit :: Statement (Id Unit) [MessageInfo NewSubpart]
selectMessagesForUnit = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      SELECT
        m.id, m.type, m.status, m.message, m.child, m.parent, u1.name, u2.name
      FROM
        messages_subpart m
      JOIN
        units u1 ON u1.id = m.child
      JOIN
        units u2 ON u2.id = m.parent
      WHERE
        m.status = 'Waiting' AND (
          (m.child = $1 AND m.type = 'Invitation') OR
          (m.parent = $1 AND m.type = 'Submission')
        )
      |]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      id <- Decoder.id
      messageType <- Decoder.messageType
      status <- Decoder.messageStatus
      text <- Decoder.text
      value <- do
        childId <- Decoder.id
        parentId <- Decoder.id
        pure NewSubpart {..}
      left <- Decoder.text
      right <- Decoder.text
      pure MessageInfo {..}

selectRepliesForUnit :: Statement (Id Unit) [ReplyInfo NewSubpart]
selectRepliesForUnit = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      SELECT
        r.id, r.type, m.type, r.reply, r.status, m.id, m.type, m.status, m.message, m.child, m.parent, u1.name, u2.name
      FROM
        replies_subpart r
      JOIN
        messages_subpart m ON m.id = r.message
      JOIN
        units u1 ON u1.id = m.child
      JOIN
        units u2 ON u2.id = m.parent
      WHERE
        r.status = 'NotSeen' AND (
          (m.child = $1 AND m.type = 'Submission') OR
          (m.parent = $1 AND m.type = 'Invitation')
        )
      |]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      id <- Decoder.id
      replyType <- Decoder.replyType
      messageType <- Decoder.messageType
      text <- Decoder.text
      status <- Decoder.replyStatus
      message <- do
        id <- Decoder.id
        messageType <- Decoder.messageType
        status <- Decoder.messageStatus
        text <- Decoder.text
        value <- do
          childId <- Decoder.id
          parentId <- Decoder.id
          pure NewSubpart {..}
        left <- Decoder.text
        right <- Decoder.text
        pure MessageInfo {..}
      pure ReplyInfo {..}

getUnitsForMessage :: Statement (Id Person, Id Unit) [WithId Unit]
getUnitsForMessage = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      WITH
        -- Units eligible for subpart messages
        units_user AS (
          SELECT id FROM units WHERE chair = $1
        ),

        -- Units which have the unit as parent
        units_parent AS (
          SELECT units_user.id AS id
          FROM units_user
          JOIN subparts ON units_user.id = subparts.child
          WHERE subparts.parent = $2
        ),

        -- Units which have the unit as child
        units_child AS (
          SELECT units_user.id AS id
          FROM units_user
          JOIN subparts ON units_user.id = subparts.parent
          WHERE subparts.child = $2
        ),

        -- Units which have pending messages as parent
        units_parent_message AS (
          SELECT units_user.id AS id
          FROM units_user
          JOIN messages_subpart ON units_user.id = messages_subpart.child
          WHERE messages_subpart.parent = $2 AND status = 'Waiting'
        ),

        -- Units which have pending messages as child
        units_child_message AS (
          SELECT units_user.id AS id
          FROM units_user
          JOIN messages_subpart ON units_user.id = messages_subpart.parent
          WHERE messages_subpart.child = $2 AND status = 'Waiting'
        )

      SELECT id, name, description, chair, image, created_at
      FROM units
      WHERE chair = $1 AND id != $2 AND id NOT IN (
        SELECT id FROM units_parent UNION
        SELECT id FROM units_child UNION
        SELECT id FROM units_parent_message UNION
        SELECT id FROM units_child_message
      )
      |]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.id

    decoder = Decoder.rowList $ do
      id <- Decoder.id
      value <- do
        name <- Decoder.text
        description <- Decoder.text
        chairId <- Decoder.id
        image <- Decoder.text
        createdAt <- Decoder.posixTime
        pure Unit {..}
      pure WithId {..}
