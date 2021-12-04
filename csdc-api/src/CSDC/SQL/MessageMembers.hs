{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.MessageMembers
  ( sendMessage
  , selectMember
  , updateMessage
  , sendReply
  , viewReply
  , Filter (..)
  , selectMessages
  , selectReplies
  , isMembershipPending
  , getUnitsForMessage
  ) where

import CSDC.Prelude
import CSDC.SQL.QQ

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString

viewReply :: Statement (Id (Reply NewMember)) ()
viewReply = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "UPDATE replies_member"
      , "SET status = 'Seen' :: reply_status"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.noResult

selectMember :: Statement (Id (Message NewMember)) (Maybe NewMember)
selectMember =  Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT person, unit"
      , "FROM messages_member"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe $
      NewMember <$>
        Decoder.id <*>
        Decoder.id

sendMessage :: Statement (NewMessage NewMember) (Id (Message NewMember))
sendMessage = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO messages_member (type, message, person, unit)"
      , "VALUES ($1 :: message_type, $2, $3, $4)"
      , "RETURNING id"
      ]

    encoder =
      (contramap newMessage_type Encoder.messageType) <>
      (contramap newMessage_text Encoder.text) <>
      (contramap (newMember_person . newMessage_value) Encoder.id) <>
      (contramap (newMember_unit . newMessage_value) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

updateMessage :: Statement (Id (Message NewMember), MessageStatus) ()
updateMessage = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "UPDATE messages_member"
      , "SET status = $2 :: message_status"
      , "WHERE id = $1"
      ]

    encoder =
      contramap fst Encoder.id <>
      contramap snd Encoder.messageStatus

    decoder = Decoder.noResult


sendReply :: Statement (NewReply NewMember) (Id (Reply NewMember))
sendReply = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO replies_member (type, reply, message)"
      , "VALUES ($1 :: reply_type, $2, $3)"
      , "RETURNING id"
      ]

    encoder =
      (contramap newReply_type Encoder.replyType) <>
      (contramap newReply_text Encoder.text) <>
      (contramap newReply_message Encoder.id)

    decoder = Decoder.singleRow Decoder.id

data Filter = Filter
  { filter_person :: Maybe (Id Person)
  , filter_unit :: Maybe (Id Unit)
  }

selectMessages :: Statement Filter [MessageInfo NewMember]
selectMessages = Statement sql encoder decoder True
  where
    sql = [sqlqq|
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
      contramap filter_person Encoder.idNullable <>
      contramap filter_unit Encoder.idNullable

    decoder = Decoder.rowList $ do
      messageInfo_id <- Decoder.id
      messageInfo_type <- Decoder.messageType
      messageInfo_status <- Decoder.messageStatus
      messageInfo_text <- Decoder.text
      messageInfo_value <- do
        newMember_person <- Decoder.id
        newMember_unit <- Decoder.id
        pure NewMember {..}
      messageInfo_left <- Decoder.text
      messageInfo_right <- Decoder.text
      pure MessageInfo {..}

selectReplies :: Statement Filter [ReplyInfo NewMember]
selectReplies = Statement sql encoder decoder True
  where
    sql = [sqlqq|
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
      contramap filter_person Encoder.idNullable <>
      contramap filter_unit Encoder.idNullable

    decoder = Decoder.rowList $ do
      replyInfo_id <- Decoder.id
      replyInfo_type <- Decoder.replyType
      replyInfo_mtype <- Decoder.messageType
      replyInfo_text <- Decoder.text
      replyInfo_status <- Decoder.replyStatus
      replyInfo_message <- do
        messageInfo_id <- Decoder.id
        messageInfo_type <- Decoder.messageType
        messageInfo_status <- Decoder.messageStatus
        messageInfo_text <- Decoder.text
        messageInfo_value <- do
          newMember_person <- Decoder.id
          newMember_unit <- Decoder.id
          pure NewMember {..}
        messageInfo_left <- Decoder.text
        messageInfo_right <- Decoder.text
        pure MessageInfo {..}
      pure ReplyInfo {..}


isMembershipPending :: Statement (Id Person, Id Unit) Bool
isMembershipPending =  Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT EXISTS ("
      , "SELECT 1"
      , "FROM messages_member"
      , "WHERE person = $1 AND unit = $2 AND status = 'Waiting'"
      , ")"
      ]

    encoder =
      contramap fst Encoder.id <>
      contramap snd Encoder.id

    decoder = Decoder.singleRow Decoder.bool

getUnitsForMessage :: Statement (Id Person, Id Person) [WithId Unit]
getUnitsForMessage = Statement sql encoder decoder True
  where
    sql = [sqlqq|
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

      SELECT id, name, description, chair, 'files/' || image, created_at
      FROM units
      WHERE chair = $1 AND id NOT IN (
        SELECT id FROM units_member UNION
        SELECT id FROM units_member_message
      )
      |]

    encoder =
      contramap fst Encoder.id <>
      contramap snd Encoder.id

    decoder = Decoder.rowList $ do
      withId_id <- Decoder.id
      withId_value <- do
        unit_name <- Decoder.text
        unit_description <- Decoder.text
        unit_chair <- Decoder.id
        unit_image <- Decoder.text
        unit_createdAt <- Decoder.timestamptz
        pure Unit {..}
      pure WithId {..}
