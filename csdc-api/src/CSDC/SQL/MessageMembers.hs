{-# LANGUAGE OverloadedStrings #-}

module CSDC.SQL.MessageMembers
  ( sendMessage
  , sendReply
  , viewReply
  ) where

import CSDC.DAO.Types (Message (..), Reply (..), Member (..))
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

sendMessage :: Statement (Message Member) (Id (Message Member))
sendMessage = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO messages_member (mtype, mstatus, message, person, unit)"
      , "VALUES ($1, $2, $3, $4, $5)"
      , "RETURNING id"
      ]

    encoder =
      (contramap message_type Encoder.messageType) <>
      (contramap message_status Encoder.messageStatus) <>
      (contramap message_text Encoder.text) <>
      (contramap (member_person . message_value) Encoder.id) <>
      (contramap (member_unit . message_value) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

sendReply :: Statement (Reply Member) (Id (Reply Member))
sendReply = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO replies_member (rtype, mtype, rstatus, reply, message)"
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
