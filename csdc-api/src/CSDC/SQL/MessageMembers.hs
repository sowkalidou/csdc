{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CSDC.SQL.MessageMembers
  ( sendMessage,
    selectMember,
    updateMessage,
    sendReply,
    viewReply,
    Filter (..),
    selectMessages,
    selectReplies,
    isMembershipPending,
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

viewReply :: Statement (Id (Reply NewMember)) ()
viewReply = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "UPDATE replies_member",
          "SET status = 'Seen' :: reply_status",
          "WHERE id = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.noResult

selectMember :: Statement (Id (Message NewMember)) (Maybe NewMember)
selectMember = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT person, unit",
          "FROM messages_member",
          "WHERE id = $1"
        ]

    encoder = Encoder.id

    decoder =
      Decoder.rowMaybe $
        NewMember
          <$> Decoder.id
          <*> Decoder.id

sendMessage :: Statement (NewMessage NewMember) (Id (Message NewMember))
sendMessage = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO messages_member (type, message, person, unit)",
          "VALUES ($1 :: message_type, $2, $3, $4)",
          "RETURNING id"
        ]

    encoder =
      (contramap (.messageType) Encoder.messageType)
        <> (contramap (.text) Encoder.text)
        <> (contramap (.value.personId) Encoder.id)
        <> (contramap (.value.unitId) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

updateMessage :: Statement (Id (Message NewMember), MessageStatus) ()
updateMessage = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "UPDATE messages_member",
          "SET status = $2 :: message_status",
          "WHERE id = $1"
        ]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.messageStatus

    decoder = Decoder.noResult

sendReply :: Statement (NewReply NewMember) (Id (Reply NewMember))
sendReply = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO replies_member (type, reply, message)",
          "VALUES ($1 :: reply_type, $2, $3)",
          "RETURNING id"
        ]

    encoder =
      (contramap (.replyType) Encoder.replyType)
        <> (contramap (.text) Encoder.text)
        <> (contramap (.messageId) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

data Filter = Filter
  { personId :: Maybe (Id Person),
    unitId :: Maybe (Id Unit)
  }

selectMessages :: Statement Filter [MessageInfo NewMember]
selectMessages = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      SELECT
        m.id, m.type, m.status, m.message, m.person, m.unit, p.name, u.name
      FROM
        messages_member m
      JOIN
        persons p ON p.id = m.person
      JOIN
        units u ON u.id = m.unit
      WHERE
        m.status = 'Waiting' AND (
          COALESCE(m.person = $1 AND m.type = 'Invitation', FALSE) OR
          COALESCE(m.unit = $2 AND m.type = 'Submission', FALSE)
        )
      |]

    encoder =
      contramap (.personId) Encoder.idNullable
        <> contramap (.unitId) Encoder.idNullable

    decoder = Decoder.rowList $ do
      id <- Decoder.id
      messageType <- Decoder.messageType
      status <- Decoder.messageStatus
      text <- Decoder.text
      value <- do
        personId <- Decoder.id
        unitId <- Decoder.id
        pure NewMember {..}
      left <- Decoder.text
      right <- Decoder.text
      pure MessageInfo {..}

selectReplies :: Statement Filter [ReplyInfo NewMember]
selectReplies = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      SELECT
        r.id, r.type, m.type, r.reply, r.status, m.id, m.type, m.status, m.message, m.person, m.unit, p.name, u.name
      FROM
        replies_member r
      JOIN
        messages_member m ON m.id = r.message
      JOIN
        persons p ON p.id = m.person
      JOIN
        units u ON u.id = m.unit
      WHERE
        r.status = 'NotSeen' AND (
          COALESCE(m.person = $1 AND m.type = 'Submission', FALSE) OR
          COALESCE(m.unit = $1 AND m.type = 'Invitation', FALSE)
        )
      |]

    encoder =
      contramap (.personId) Encoder.idNullable
        <> contramap (.unitId) Encoder.idNullable

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
          personId <- Decoder.id
          unitId <- Decoder.id
          pure NewMember {..}
        left <- Decoder.text
        right <- Decoder.text
        pure MessageInfo {..}
      pure ReplyInfo {..}

isMembershipPending :: Statement (Id Person, Id Unit) Bool
isMembershipPending = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT EXISTS (",
          "SELECT 1",
          "FROM messages_member",
          "WHERE person = $1 AND unit = $2 AND status = 'Waiting'",
          ")"
        ]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.id

    decoder = Decoder.singleRow Decoder.bool

getUnitsForMessage :: Statement (Id Person, Id Person) [WithId Unit]
getUnitsForMessage = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      WITH
        -- Units eligible for member messages
        units_user AS (
          SELECT id FROM units WHERE chair = $1
        ),

        -- Units which have the unit as member
        units_member AS (
          SELECT units_user.id AS id
          FROM units_user
          JOIN members ON units_user.id = members.unit
          WHERE members.person = $2
        ),

        -- Units which have pending messages
        units_member_message AS (
          SELECT units_user.id AS id
          FROM units_user
          JOIN messages_member ON units_user.id = messages_member.unit
          WHERE messages_member.person = $2 AND status = 'Waiting'
        )

      SELECT id, name, description, chair, image, created_at
      FROM units
      WHERE chair = $1 AND id NOT IN (
        SELECT id FROM units_member UNION
        SELECT id FROM units_member_message
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
