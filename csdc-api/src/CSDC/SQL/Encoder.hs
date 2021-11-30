module CSDC.SQL.Encoder
  ( -- * Base types
    bytea
  , ctime
  , int
  , text
  , textNullable
  , textList
    -- * Local types
  , id
  , idNullable
  , idList
  , orcidId
  , messageType
  , messageStatus
  , replyType
  , replyStatus
  ) where

import CSDC.Prelude
import Prelude hiding (id)

import qualified CSDC.Auth.ORCID as ORCID

import Data.ByteString (ByteString)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Text (Text)
import Foreign.C.Types (CTime (..))
import Hasql.Encoders
  (Params, param, nonNullable, nullable, foldableArray)

import qualified Hasql.Encoders as Encoders

--------------------------------------------------------------------------------
-- Base types

bytea :: Params ByteString
bytea = param (nonNullable Encoders.bytea)

ctime :: Params CTime
ctime = contramap (\(CTime t) -> t) $ param (nonNullable Encoders.int8)

int :: Params Int
int = contramap fromIntegral $ param (nonNullable Encoders.int8)

text :: Params Text
text = param (nonNullable Encoders.text)

textNullable :: Params (Maybe Text)
textNullable = param (nullable Encoders.text)

textList :: Params [Text]
textList = param $ nonNullable $ foldableArray $ nonNullable Encoders.text

--------------------------------------------------------------------------------
-- Local types

id :: Params (Id a)
id =
  contramap (fromIntegral . getId) $
  param (nonNullable Encoders.int4)

idNullable :: Params (Maybe (Id a))
idNullable =
  contramap (fmap (fromIntegral . getId)) $
  param (nullable Encoders.int4)

idList :: Params [Id a]
idList =
  contramap (fmap (fromIntegral . getId)) $
  param (nonNullable (foldableArray (nonNullable Encoders.int4)))

orcidId :: Params ORCID.Id
orcidId =
  contramap ORCID.getId $
  param (nonNullable Encoders.text)

messageType :: Params MessageType
messageType = contramap encode text
  where
    encode Invitation = "Invitation"
    encode Submission = "Submission"

messageStatus :: Params MessageStatus
messageStatus = contramap encode text
  where
    encode Waiting = "Waiting"
    encode Accepted = "Accepted"
    encode Rejected = "Rejected"

replyType :: Params ReplyType
replyType = contramap encode text
  where
    encode Accept = "Accept"
    encode Reject = "Reject"

replyStatus :: Params ReplyStatus
replyStatus = contramap encode text
  where
    encode Seen = "Seen"
    encode NotSeen = "NotSeen"
