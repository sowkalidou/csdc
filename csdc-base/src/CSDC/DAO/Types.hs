{-# LANGUAGE StrictData #-}

module CSDC.DAO.Types
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
    -- GUI Types
  , PersonMember (..)
  , PersonInfo (..)
  , UnitMember (..)
  , UnitSubpart (..)
  , UnitInfo (..)
  , Inbox (..)
  , MessageInfo (..)
  , ReplyInfo (..)
    -- Reexport
  , Id (..)
  ) where

import CSDC.Aeson (JSON (..))
import CSDC.Data.Id (Id (..))

import qualified CSDC.Auth.ORCID as ORCID

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Fundamental types

data Person = Person
  { person_name :: Text
  , person_description :: Text
  , person_orcid :: ORCID.Id
  , person_image :: Maybe Text
  , person_createdAt :: UTCTime
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Person

data NewPerson = NewPerson
  { newPerson_name :: Text
  , newPerson_description :: Text
  , newPerson_orcid :: ORCID.Id
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON NewPerson

data PersonUpdate = PersonUpdate
  { personUpdate_name :: Text
  , personUpdate_description :: Text
  , personUpdate_image :: Maybe Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON PersonUpdate

data Unit = Unit
  { unit_name :: Text
  , unit_description :: Text
  , unit_chair :: Id Person
  , unit_createdAt :: UTCTime
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
  , member_createdAt :: UTCTime
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
  , subpart_createdAt :: UTCTime
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
  , message_createdAt :: UTCTime
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
  , reply_createdAt :: UTCTime
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

data PersonMember = PersonMember
  { personMember_member :: Id Member
  , personMember_id :: Id Unit
  , personMember_unit :: Unit
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON PersonMember

data PersonInfo = PersonInfo
  { personInfo_id :: Id Person
  , personInfo_person :: Person
  , personInfo_members :: [PersonMember]
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON PersonInfo

data UnitMember = UnitMember
  { unitMember_member :: Id Member
  , unitMember_id :: Id Person
  , unitMember_person :: Person
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON UnitMember

data UnitSubpart = UnitSubpart
  { unitSubpart_subpart :: Id Subpart
  , unitSubpart_id :: Id Unit
  , unitSubpart_unit :: Unit
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON UnitSubpart

data UnitInfo = UnitInfo
  { unitInfo_id :: Id Unit
  , unitInfo_unit :: Unit
  , unitInfo_members :: [UnitMember]
  , unitInfo_children :: [UnitSubpart]
  , unitInfo_parents :: [UnitSubpart]
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON UnitInfo

data MessageInfo a = MessageInfo
  { messageInfo_id :: Id (Message a)
  , messageInfo_type :: MessageType
  , messageInfo_status :: MessageStatus
  , messageInfo_text :: Text
  , messageInfo_value :: a
  , messageInfo_left :: Text
  , messageInfo_right :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (MessageInfo a)

data ReplyInfo a = ReplyInfo
  { replyInfo_id :: Id (Reply a)
  , replyInfo_type :: ReplyType
  , replyInfo_mtype :: MessageType
  , replyInfo_text :: Text
  , replyInfo_status :: ReplyStatus
  , replyInfo_message :: MessageInfo a
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (ReplyInfo a)

data Inbox = Inbox
  { inbox_messageMember :: [MessageInfo NewMember]
  , inbox_replyMember :: [ReplyInfo NewMember]
  , inbox_messageSubpart :: [MessageInfo NewSubpart]
  , inbox_replySubpart :: [ReplyInfo NewSubpart]
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Inbox
