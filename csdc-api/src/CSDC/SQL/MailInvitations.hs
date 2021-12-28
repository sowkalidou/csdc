{-# LANGUAGE QuasiQuotes #-}

module CSDC.SQL.MailInvitations
  ( insert
  , select
  , delete
  ) where

import CSDC.Prelude
import CSDC.SQL.QQ

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Hasql.Decoders as Decoders

--------------------------------------------------------------------------------
-- Queries

insert :: Statement (Id Unit, Text) ()
insert = Statement sql encoder Decoders.noResult True
  where
    sql = [sqlqq|
      INSERT INTO member_email_invitations
        ( unit
        , email
        )
      VALUES ($1, $2)
      |]

    encoder =
      contramap fst Encoder.id <>
      contramap snd Encoder.text

delete :: Statement Text ()
delete = Statement sql encoder Decoders.noResult True
  where
    sql = [sqlqq|
      DELETE FROM member_email_invitations
      WHERE email = $1
      |]

    encoder = Encoder.text

select :: Statement Text [Id Unit]
select = Statement sql encoder decoder True
  where
    sql = [sqlqq|
      SELECT unit
      FROM member_email_invitations
      WHERE email = $1
      |]

    encoder = Encoder.text

    decoder = Decoders.rowList Decoder.id
