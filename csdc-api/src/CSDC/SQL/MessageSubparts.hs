{-# LANGUAGE OverloadedStrings #-}

module CSDC.SQL.MessageSubparts
  ( sendMessage
  , sendReply
  , viewReply
  , unitMessages
  , unitReplies
  ) where

import CSDC.DAO.Types (Unit (..), Message (..), Reply (..), Subpart (..))
import CSDC.Data.Id (Id (..), WithId (..))

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

sendMessage :: Statement (Message Subpart) (Id (Message Subpart))
sendMessage = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO messages_subpart (mtype, mstatus, message, child, parent)"
      , "VALUES ($1, $2, $3, $4, $5)"
      , "RETURNING id"
      ]

    encoder =
      (contramap message_type Encoder.messageType) <>
      (contramap message_status Encoder.messageStatus) <>
      (contramap message_text Encoder.text) <>
      (contramap (subpart_child . message_value) Encoder.id) <>
      (contramap (subpart_parent . message_value) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

sendReply :: Statement (Reply Subpart) (Id (Reply Subpart))
sendReply = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO replies_subpart (rtype, mtype, rstatus, reply, message)"
      , "VALUES ($1, $2, $3, $4, $5)"
      , "RETURNING id"
      ]

    encoder =
      (contramap reply_type Encoder.replyType) <>
      (contramap reply_mtype Encoder.messageType) <>
      (contramap reply_status Encoder.replyStatus) <>
      (contramap reply_text Encoder.text) <>
      (contramap reply_id Encoder.id)

    decoder = Decoder.singleRow Decoder.id

unitMessages :: Statement (Id Unit) [WithId (Message Subpart)]
unitMessages = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT id, mtype, mstatus, message, child, parent"
      , "FROM messages_subpart"
      , "WHERE unit = $1"
      ]

    encoder = Encoder.id

    makeMessage uid ty tx st p u =
      WithId uid (Message ty tx st (Subpart p u))

    decoder = Decoder.rowList $
      makeMessage <$>
        Decoder.id <*>
        Decoder.messageType <*>
        Decoder.text <*>
        Decoder.messageStatus <*>
        Decoder.id <*>
        Decoder.id

unitReplies :: Statement [Id (Message Subpart)] [WithId (Reply Subpart)]
unitReplies = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT id, rtype, mtype, rstatus, reply, message"
      , "FROM replies_subpart"
      , "WHERE message = ANY($1)"
      ]

    encoder = Encoder.idList

    makeReply uid ty mty tx st mid =
      WithId uid (Reply ty mty tx st mid)

    decoder = Decoder.rowList $
      makeReply <$>
        Decoder.id <*>
        Decoder.replyType <*>
        Decoder.messageType <*>
        Decoder.text <*>
        Decoder.replyStatus <*>
        Decoder.id

