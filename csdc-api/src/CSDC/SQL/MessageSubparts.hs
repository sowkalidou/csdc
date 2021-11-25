{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.MessageSubparts
  ( sendMessage
  , updateMessage
  , selectSubpart
  , sendReply
  , viewReply
  , select
  , messageReplies
  ) where

import CSDC.DAO.Types
  ( Unit (..)
  , Message (..)
  , MessageInfo (..)
  , MessageStatus
  , Reply (..)
  , ReplyInfo (..)
  , Subpart (..)
  )
import CSDC.Data.Id (Id (..))

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString

viewReply :: Statement (Id (Reply Subpart)) ()
viewReply = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "UPDATE replies_subpart"
      , "SET status = 'Seen' :: reply_status"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.noResult

selectSubpart :: Statement (Id (Message Subpart)) (Maybe Subpart)
selectSubpart =  Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT child, parent"
      , "FROM messages_subpart"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe $
      Subpart <$>
        Decoder.id <*>
        Decoder.id

sendMessage :: Statement (Message Subpart) (Id (Message Subpart))
sendMessage = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO messages_subpart (type, status, message, child, parent)"
      , "VALUES ($1 :: message_type, 'Pending' :: message_status, $3, $4, $5)"
      , "RETURNING id"
      ]

    encoder =
      (contramap message_type Encoder.messageType) <>
      (contramap message_text Encoder.text) <>
      (contramap (subpart_child . message_value) Encoder.id) <>
      (contramap (subpart_parent . message_value) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

updateMessage :: Statement (Id (Message Subpart), MessageStatus) ()
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

sendReply :: Statement (Reply Subpart) (Id (Reply Subpart))
sendReply = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO replies_subpart (type, status, reply, message)"
      , "VALUES ($1 :: reply_type, $2 :: reply_status, $3, $4)"
      , "RETURNING id"
      ]

    encoder =
      (contramap reply_type Encoder.replyType) <>
      (contramap reply_status Encoder.replyStatus) <>
      (contramap reply_text Encoder.text) <>
      (contramap reply_id Encoder.id)

    decoder = Decoder.singleRow Decoder.id

select :: Statement (Id Unit) [(Id (Message Subpart), MessageInfo Subpart)]
select = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT m.id, m.type, m.status, m.message, m.child, m.parent, u1.name, u2.name"
      , "FROM"
      , "  messages_subpart m"
      , "JOIN"
      , "  units u1 ON u1.id = m.child"
      , "JOIN"
      , "  units u2 ON u2.id = m.parent"
      , "WHERE m.child = $1 OR m.parent = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      uid <- Decoder.id
      messageInfo_type <- Decoder.messageType
      messageInfo_status <- Decoder.messageStatus
      messageInfo_text <- Decoder.text
      messageInfo_value <- do
        subpart_child <- Decoder.id
        subpart_parent <- Decoder.id
        pure Subpart {..}
      messageInfo_left <- Decoder.text
      messageInfo_right <- Decoder.text
      pure (uid, MessageInfo {..})

messageReplies :: Statement [Id (Message Subpart)] [(Id (Reply Subpart), ReplyInfo Subpart)]
messageReplies = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT r.id, r.type, m.type, r.reply, r.status, m.type, m.status, m.message, m.child, m.parent, u1.name, u2.name"
      , "FROM"
      , "  replies_subpart r"
      , "JOIN"
      , "  messages_subpart m ON m.id = r.message"
      , "JOIN"
      , "  units u1 ON u1.id = m.child"
      , "JOIN"
      , "  units u2 ON u2.id = m.parent"
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
          subpart_child <- Decoder.id
          subpart_parent <- Decoder.id
          pure Subpart {..}
        messageInfo_left <- Decoder.text
        messageInfo_right <- Decoder.text
        pure MessageInfo {..}
      pure (uid, ReplyInfo {..})
