module CSDC.SQL.Encoder
  ( -- * Base types
    bytea,
    int,
    text,
    textNullable,
    textList,

    -- * Local types
    id,
    idNullable,
    idList,
    messageType,
    messageStatus,
    replyType,
    replyStatus,
  )
where

import CSDC.Prelude
import Data.ByteString (ByteString)
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Encoders
  ( Params,
    foldableArray,
    nonNullable,
    nullable,
    param,
  )
import Hasql.Encoders qualified as Encoders
import Prelude hiding (id)

--------------------------------------------------------------------------------
-- Base types

bytea :: Params ByteString
bytea = param (nonNullable Encoders.bytea)

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
  contramap getId $
    param (nonNullable Encoders.uuid)

idNullable :: Params (Maybe (Id a))
idNullable =
  contramap (fmap getId) $
    param (nullable Encoders.uuid)

idList :: Params [Id a]
idList =
  contramap (fmap getId) $
    param (nonNullable (foldableArray (nonNullable Encoders.uuid)))

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
