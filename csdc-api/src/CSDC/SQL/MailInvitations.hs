{-# LANGUAGE QuasiQuotes #-}

module CSDC.SQL.MailInvitations
  ( insert,
    select,
    delete,
  )
where

import CSDC.Prelude
import CSDC.SQL.Decoder qualified as Decoder
import CSDC.SQL.Encoder qualified as Encoder
import CSDC.SQL.QQ
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Decoders qualified as Decoders
import Hasql.Statement (Statement (..))

--------------------------------------------------------------------------------
-- Queries

insert :: Statement (Id Unit, Text) ()
insert = Statement sql encoder Decoders.noResult True
  where
    sql =
      [sqlqq|
      INSERT INTO member_email_invitations
        ( unit
        , email
        )
      VALUES ($1, $2)
      |]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.text

delete :: Statement Text ()
delete = Statement sql encoder Decoders.noResult True
  where
    sql =
      [sqlqq|
      DELETE FROM member_email_invitations
      WHERE email = $1
      |]

    encoder = Encoder.text

select :: Statement Text [Id Unit]
select = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
      SELECT unit
      FROM member_email_invitations
      WHERE email = $1
      |]

    encoder = Encoder.text

    decoder = Decoders.rowList Decoder.id
