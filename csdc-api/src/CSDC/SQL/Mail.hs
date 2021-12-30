{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.Mail
  ( insert
  , select
  , delete
  ) where

import CSDC.Prelude
import CSDC.Mail (Mail(..))
import CSDC.SQL.QQ

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Data.Maybe (fromMaybe)
import Hasql.Statement (Statement (..))
import Network.Mail.Mime (Address (..))

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

--------------------------------------------------------------------------------
-- Queries

insert :: Statement Mail ()
insert = Statement sql encoder Decoders.noResult True
  where
    sql = [sqlqq|
      INSERT INTO emails
        ( from_name
        , from_address
        , to_addresses
        , subject
        , text
        , html
        )
      VALUES ($1, $2, $3, $4, $5, $6)
      |]

    encoder =
      contramap (fromMaybe "" . addressName . from) Encoder.text <>
      contramap (addressEmail . from) Encoder.text <>
      contramap (fmap addressEmail . to) Encoder.textList <>
      contramap subject Encoder.text <>
      contramap text Encoder.text <>
      contramap html Encoder.text

delete :: Statement (Id Mail) ()
delete = Statement sql encoder Decoders.noResult True
  where
    sql = [sqlqq|
      DELETE FROM emails
      WHERE id = $1
      |]

    encoder = Encoder.id

select :: Statement () [(Id Mail, Mail)]
select = Statement sql Encoders.noParams decoder True
  where
    sql = [sqlqq|
      SELECT
        id,
        from_name,
        from_address,
        to_addresses,
        subject,
        text,
        html
      FROM
        emails
      |]

    decoder = Decoders.rowList $ do
      uuid <- Decoder.id @Mail
      fromName <- Decoder.text
      fromAddress <- Decoder.text
      let from = Address (Just fromName) fromAddress
      toAddresses <- Decoder.textList
      let to = fmap (\a -> Address Nothing a) toAddresses
      subject <- Decoder.text
      text <- Decoder.text
      html <- Decoder.text

      pure (uuid, Mail {..})
