{-# LANGUAGE StrictData #-}

module CSDC.Types.DAO
  ( -- Person
    Person (..)
  , NewPerson (..)
  , PersonUpdate (..)
    -- Units
  , Unit (..)
  , NewUnit (..)
  , UnitUpdate (..)
    -- Member
  , Member (..)
  , NewMember (..)
    -- Subpart
  , Subpart (..)
  , NewSubpart (..)
    -- Messages
  , Message (..)
  , NewMessage (..)
  , MessageStatus (..)
  , MessageType (..)
    -- Reply
  , Reply (..)
  , NewReply (..)
  , ReplyStatus (..)
  , ReplyType (..)
  ) where

import CSDC.Aeson (JSON (..))
import CSDC.Types.Id

import Data.Aeson (ToJSON, FromJSON)
import Data.Password.Bcrypt (PasswordHash, Bcrypt)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Fundamental types

data Person = Person
  { person_name :: Text
  , person_description :: Text
  , person_image :: Text
  , person_email :: Text
  , person_createdAt :: POSIXTime
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Person

data NewPerson = NewPerson
  { newPerson_name :: Text
  , newPerson_description :: Text
  , newPerson_image :: Text
  , newPerson_email :: Text
  , newPerson_password :: PasswordHash Bcrypt
  } deriving (Show, Eq, Generic)

data PersonUpdate = PersonUpdate
  { personUpdate_name :: Text
  , personUpdate_description :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON PersonUpdate

data Unit = Unit
  { unit_name :: Text
  , unit_description :: Text
  , unit_chair :: Id Person
  , unit_image :: Text
  , unit_createdAt :: POSIXTime
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Unit

data NewUnit = NewUnit
  { newUnit_name :: Text
  , newUnit_description :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON NewUnit

data UnitUpdate = UnitUpdate
  { unitUpdate_name :: Text
  , unitUpdate_description :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON UnitUpdate

--------------------------------------------------------------------------------
-- Fundamental relations

data Member = Member
  { member_person :: Id Person
  , member_unit :: Id Unit
  , member_createdAt :: POSIXTime
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Member

data NewMember = NewMember
  { newMember_person :: Id Person
  , newMember_unit :: Id Unit
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON NewMember

data Subpart = Subpart
  { subpart_child :: Id Unit
  , subpart_parent :: Id Unit
  , subpart_createdAt :: POSIXTime
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Subpart

data NewSubpart = NewSubpart
  { newSubpart_child :: Id Unit
  , newSubpart_parent :: Id Unit
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON NewSubpart

--------------------------------------------------------------------------------
-- Messages

data MessageStatus = Waiting | Accepted | Rejected
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON MessageStatus

data MessageType = Invitation | Submission
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON MessageType

data Message a = Message
  { message_type :: MessageType
  , message_text :: Text
  , message_status :: MessageStatus
  , message_value :: a
  , message_createdAt :: POSIXTime
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (Message a)

data NewMessage a = NewMessage
  { newMessage_type :: MessageType
  , newMessage_text :: Text
  , newMessage_value :: a
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (NewMessage a)

data ReplyStatus = Seen | NotSeen
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON ReplyStatus

data ReplyType = Accept | Reject
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON ReplyType

data Reply a = Reply
  { reply_type :: ReplyType
  , reply_mtype :: MessageType
  , reply_text :: Text
  , reply_status :: ReplyStatus
  , reply_id :: Id (Message a)
  , reply_createdAt :: POSIXTime
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (Reply a)

data NewReply a = NewReply
  { newReply_type :: ReplyType
  , newReply_text :: Text
  , newReply_message :: Id (Message a)
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (NewReply a)
