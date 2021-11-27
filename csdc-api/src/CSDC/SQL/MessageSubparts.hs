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

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString

viewReply :: Statement (Id (Reply NewSubpart)) ()
viewReply = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "UPDATE replies_subpart"
      , "SET status = 'Seen' :: reply_status"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.noResult

selectSubpart :: Statement (Id (Message NewSubpart)) (Maybe NewSubpart)
selectSubpart =  Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT child, parent"
      , "FROM messages_subpart"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe $
      NewSubpart <$>
        Decoder.id <*>
        Decoder.id

sendMessage :: Statement (NewMessage NewSubpart) (Id (Message NewSubpart))
sendMessage = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO messages_subpart (type, message, child, parent)"
      , "VALUES ($1 :: message_type, $2, $3, $4)"
      , "RETURNING id"
      ]

    encoder =
      (contramap newMessage_type Encoder.messageType) <>
      (contramap newMessage_text Encoder.text) <>
      (contramap (newSubpart_child . newMessage_value) Encoder.id) <>
      (contramap (newSubpart_parent . newMessage_value) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

updateMessage :: Statement (Id (Message NewSubpart), MessageStatus) ()
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

sendReply :: Statement (NewReply NewSubpart) (Id (Reply NewSubpart))
sendReply = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO replies_subpart (type, reply, message)"
      , "VALUES ($1 :: reply_type, $2, $3)"
      , "RETURNING id"
      ]

    encoder =
      (contramap newReply_type Encoder.replyType) <>
      (contramap newReply_text Encoder.text) <>
      (contramap newReply_message Encoder.id)

    decoder = Decoder.singleRow Decoder.id

select :: Statement (Id Unit) [MessageInfo NewSubpart]
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
      messageInfo_id <- Decoder.id
      messageInfo_type <- Decoder.messageType
      messageInfo_status <- Decoder.messageStatus
      messageInfo_text <- Decoder.text
      messageInfo_value <- do
        newSubpart_child <- Decoder.id
        newSubpart_parent <- Decoder.id
        pure NewSubpart {..}
      messageInfo_left <- Decoder.text
      messageInfo_right <- Decoder.text
      pure MessageInfo {..}

messageReplies :: Statement [Id (Message NewSubpart)] [ReplyInfo NewSubpart]
messageReplies = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT r.id, r.type, m.type, r.reply, r.status, m.id, m.type, m.status, m.message, m.child, m.parent, u1.name, u2.name"
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
          newSubpart_child <- Decoder.id
          newSubpart_parent <- Decoder.id
          pure NewSubpart {..}
        messageInfo_left <- Decoder.text
        messageInfo_right <- Decoder.text
        pure MessageInfo {..}
      pure ReplyInfo {..}
