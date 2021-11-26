{-# LANGUAGE StrictData #-}

module CSDC.DAO.Types
  ( -- Entities
    Person (..)
  , PersonUpdate (..)
  , Unit (..)
  , NewUnit (..)
  , UnitUpdate (..)
    -- Relations
  , Member (..)
  , Subpart (..)
    -- Messages
  , Message (..)
  , NewMessage (..)
  , MessageStatus (..)
  , MessageType (..)
  , Reply (..)
  , NewReply (..)
  , ReplyStatus (..)
  , ReplyType (..)
    -- GUI Types
  , PersonInfo (..)
  , UnitInfo (..)
  , Inbox (..)
  , MessageInfo (..)
  , ReplyInfo (..)
  ) where

import CSDC.Aeson (JSON (..))
import CSDC.Data.Id (Id, WithId)
import CSDC.Data.IdMap (IdMap)

import qualified CSDC.Auth.ORCID as ORCID

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Fundamental types

data Person = Person
  { person_name :: Text
  , person_description :: Text
  , person_orcid :: ORCID.Id
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Person

data PersonUpdate = PersonUpdate
  { personUpdate_name :: Text
  , personUpdate_description :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON PersonUpdate

data Unit = Unit
  { unit_name :: Text
  , unit_description :: Text
  , unit_chair :: Id Person
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
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Member

data Subpart = Subpart
  { subpart_child :: Id Unit
  , subpart_parent :: Id Unit
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Subpart

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
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (Reply a)

data NewReply a = NewReply
  { newReply_type :: ReplyType
  , newReply_text :: Text
  , newReply_message :: Id (Message a)
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (NewReply a)

--------------------------------------------------------------------------------
-- GUI Types

data PersonInfo = PersonInfo
  { personInfo_id :: Id Person
  , personInfo_person :: Person
  , personInfo_members :: IdMap Member (WithId Unit)
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON PersonInfo

data UnitInfo = UnitInfo
  { unitInfo_id :: Id Unit
  , unitInfo_unit :: Unit
  , unitInfo_members :: IdMap Member (WithId Person)
  , unitInfo_children :: IdMap Subpart (WithId Unit)
  , unitInfo_parents :: IdMap Subpart (WithId Unit)
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON UnitInfo

data MessageInfo a = MessageInfo
  { messageInfo_type :: MessageType
  , messageInfo_status :: MessageStatus
  , messageInfo_text :: Text
  , messageInfo_value :: a
  , messageInfo_left :: Text
  , messageInfo_right :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (MessageInfo a)

data ReplyInfo a = ReplyInfo
  { replyInfo_type :: ReplyType
  , replyInfo_mtype :: MessageType
  , replyInfo_text :: Text
  , replyInfo_status :: ReplyStatus
  , replyInfo_message :: MessageInfo a
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (ReplyInfo a)

data Inbox = Inbox
  { inbox_messageMember :: IdMap (Message Member) (MessageInfo Member)
  , inbox_replyMember :: IdMap (Reply Member) (ReplyInfo Member)
  , inbox_messageSubpart :: IdMap (Message Subpart) (MessageInfo Subpart)
  , inbox_replySubpart :: IdMap (Reply Subpart) (ReplyInfo Subpart)
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Inbox
