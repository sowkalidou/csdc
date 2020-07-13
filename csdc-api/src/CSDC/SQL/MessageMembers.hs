{-# LANGUAGE OverloadedStrings #-}

module CSDC.SQL.MessageMembers
  ( sendMessage
  , sendReply
  , viewReply
  , Filter (..)
  , select
  , messageReplies
  ) where

import CSDC.DAO.Types (Message (..), Reply (..), Person, Unit, Member (..))
import CSDC.Data.Id (Id (..), WithId (..))

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
      , "VALUES ($1 :: message_type, $2 :: message_status, $3, $4, $5)"
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
      , "VALUES ($1 :: reply_type, $2 :: message_type, $3 :: reply_status, $4, $5)"
      , "RETURNING id"
      ]

    encoder =
      (contramap reply_type Encoder.replyType) <>
      (contramap reply_mtype Encoder.messageType) <>
      (contramap reply_status Encoder.replyStatus) <>
      (contramap reply_text Encoder.text) <>
      (contramap reply_id Encoder.id)

    decoder = Decoder.singleRow Decoder.id

data Filter = Filter
  { filter_person :: Maybe (Id Person)
  , filter_unit :: Maybe (Id Unit)
  }

select :: Statement Filter [WithId (Message Member)]
select = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT id, mtype, message, mstatus, person, unit"
      , "FROM messages_member"
      , "WHERE"
      , "  COALESCE (person = $1, TRUE)"
      , "AND"
      , "  COALESCE (unit = $2, TRUE)"
      ]

    encoder =
      contramap filter_person Encoder.idNullable <>
      contramap filter_unit Encoder.idNullable

    makeMessage uid ty tx st p u =
      WithId uid (Message ty tx st (Member p u))

    decoder = Decoder.rowList $
      makeMessage <$>
        Decoder.id <*>
        Decoder.messageType <*>
        Decoder.text <*>
        Decoder.messageStatus <*>
        Decoder.id <*>
        Decoder.id

messageReplies :: Statement [Id (Message Member)] [WithId (Reply Member)]
messageReplies = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT id, rtype, mtype, reply, rstatus, message"
      , "FROM replies_member"
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
