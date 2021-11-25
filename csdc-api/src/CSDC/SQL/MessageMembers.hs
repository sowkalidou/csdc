{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.MessageMembers
  ( sendMessage
  , selectMember
  , updateMessage
  , sendReply
  , viewReply
  , Filter (..)
  , select
  , messageReplies
  ) where

import CSDC.DAO.Types
  ( Message (..)
  , NewMessage (..)
  , MessageInfo (..)
  , MessageStatus
  , Reply (..)
  , NewReply (..)
  , ReplyInfo (..)
  , Person
  , Unit
  , Member (..)
  )
import CSDC.Data.Id (Id (..))

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString

viewReply :: Statement (Id (Reply Member)) ()
viewReply = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "UPDATE replies_member"
      , "SET status = 'Seen' :: reply_status"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.noResult

selectMember :: Statement (Id (Message Member)) (Maybe Member)
selectMember =  Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT person, unit"
      , "FROM messages_member"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe $
      Member <$>
        Decoder.id <*>
        Decoder.id

sendMessage :: Statement (NewMessage Member) (Id (Message Member))
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
      (contramap (member_person . newMessage_value) Encoder.id) <>
      (contramap (member_unit . newMessage_value) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

updateMessage :: Statement (Id (Message Member), MessageStatus) ()
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


sendReply :: Statement (NewReply Member) (Id (Reply Member))
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

select :: Statement Filter [(Id (Message Member), MessageInfo Member)]
select = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT m.id, m.type, m.status, m.message, m.person, m.unit, p.name, u.name"
      , "FROM"
      , "  messages_member m"
      , "JOIN"
      , "  persons p ON p.id = m.person"
      , "JOIN"
      , "  units u ON u.id = m.unit"
      , "WHERE"
      , "  COALESCE (person = $1, TRUE)"
      , "AND"
      , "  COALESCE (unit = $2, TRUE)"
      ]

    encoder =
      contramap filter_person Encoder.idNullable <>
      contramap filter_unit Encoder.idNullable

    decoder = Decoder.rowList $ do
      uid <- Decoder.id
      messageInfo_type <- Decoder.messageType
      messageInfo_status <- Decoder.messageStatus
      messageInfo_text <- Decoder.text
      messageInfo_value <- do
        member_person <- Decoder.id
        member_unit <- Decoder.id
        pure Member {..}
      messageInfo_left <- Decoder.text
      messageInfo_right <- Decoder.text
      pure (uid, MessageInfo {..})

messageReplies :: Statement [Id (Message Member)] [(Id (Reply Member), ReplyInfo Member)]
messageReplies = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT r.id, r.type, m.type, r.reply, r.status, m.type, m.status, m.message, m.person, m.unit, p.name, u.name"
      , "FROM"
      , "  replies_member r"
      , "JOIN"
      , "  messages_member m ON m.id = r.message"
      , "JOIN"
      , "  persons p ON p.id = m.person"
      , "JOIN"
      , "  units u ON u.id = m.unit"
      , "WHERE r.message = ANY($1)"
      ]

    encoder = Encoder.idList

    decoder = Decoder.rowList $ do
      uid <- Decoder.id
      replyInfo_type <- Decoder.replyType
      replyInfo_mtype <- Decoder.messageType
      replyInfo_text <- Decoder.text
      replyInfo_status <- Decoder.replyStatus
      replyInfo_message <- do
        messageInfo_type <- Decoder.messageType
        messageInfo_status <- Decoder.messageStatus
        messageInfo_text <- Decoder.text
        messageInfo_value <- do
          member_person <- Decoder.id
          member_unit <- Decoder.id
          pure Member {..}
        messageInfo_left <- Decoder.text
        messageInfo_right <- Decoder.text
        pure MessageInfo {..}
      pure (uid, ReplyInfo {..})
