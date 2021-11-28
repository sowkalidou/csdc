module CSDC.SQL.Decoder
  ( -- * Base types
    bool
  , text
  , textNullable
  , timestamptz
    -- * Local types
  , id
  , orcidId
  , messageType
  , messageStatus
  , replyType
  , replyStatus
    -- * Reexport
  , Decoders.rowList
  , Decoders.rowMaybe
  , Decoders.singleRow
  , Decoders.noResult
  ) where

import CSDC.Prelude
import Prelude hiding (id)

import qualified CSDC.Auth.ORCID as ORCID

import Data.Text (Text)
import Data.Time (UTCTime)
import Hasql.Decoders (Row, column, nonNullable, nullable)

import qualified Hasql.Decoders as Decoders

--------------------------------------------------------------------------------
-- Base types

bool :: Row Bool
bool = column (nonNullable Decoders.bool)

text :: Row Text
text = column (nonNullable Decoders.text)

textNullable :: Row (Maybe Text)
textNullable = column (nullable Decoders.text)

timestamptz :: Row UTCTime
timestamptz = column (nonNullable Decoders.timestamptz)

--------------------------------------------------------------------------------
-- Local types

id :: Row (Id a)
id = Id . fromIntegral <$> column (nonNullable Decoders.int4)

orcidId :: Row ORCID.Id
orcidId = ORCID.Id <$> column (nonNullable Decoders.text)

messageType :: Row MessageType
messageType = column (nonNullable (Decoders.enum decode))
  where
    decode a = lookup a
      [ ("Invitation", Invitation)
      , ("Submission", Submission)
      ]

messageStatus :: Row MessageStatus
messageStatus = column (nonNullable (Decoders.enum decode))
  where
    decode a = lookup a
      [ ("Waiting", Waiting)
      , ("Accepted", Accepted)
      , ("Rejected", Rejected)
      ]

replyType :: Row ReplyType
replyType = column (nonNullable (Decoders.enum decode))
  where
    decode a = lookup a
      [ ("Accept", Accept)
      , ("Reject", Reject)
      ]

replyStatus :: Row ReplyStatus
replyStatus = column (nonNullable (Decoders.enum decode))
  where
    decode a = lookup a
      [ ("Seen", Seen)
      , ("NotSeen", NotSeen)
      ]
