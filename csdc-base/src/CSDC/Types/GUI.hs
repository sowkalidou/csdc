{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveFunctor #-}

module CSDC.Types.GUI
  (-- GUI Types
    PersonMember (..)
  , PersonInfo (..)
  , UnitMember (..)
  , UnitSubpart (..)
  , UnitInfo (..)
  , Inbox (..)
  , MessageInfo (..)
  , ReplyInfo (..)
    -- Search
  , SearchId (..)
  , SearchResult (..)
  ) where

import CSDC.Aeson (JSON (..))
import CSDC.Types.DAO
import CSDC.Types.Id

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

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
  , personInfo_unitsForMessage :: [WithId Unit]
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
  , unitInfo_user :: Id Person
  , unitInfo_isMember :: Bool
  , unitInfo_isAdmin :: Bool
  , unitInfo_isMembershipPending :: Bool
  , unitInfo_unitsForMessage :: [WithId Unit]
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

instance Semigroup Inbox where
  Inbox mm1 rm1 ms1 rs1 <> Inbox mm2 rm2 ms2 rs2 =
    Inbox (mm1 <> mm2) (rm1 <> rm2) (ms1 <> ms2) (rs1 <> rs2)

instance Monoid Inbox where
  mempty = Inbox [] [] [] []

--------------------------------------------------------------------------------
-- Search

data SearchId = SearchPerson (Id Person) | SearchUnit (Id Unit)
    deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON SearchId

data SearchResult a = SearchResult
  { searchResult_id :: a
  , searchResult_name :: Text
  } deriving (Show, Eq, Generic, Functor)
    deriving (FromJSON, ToJSON) via JSON (SearchResult a)

--------------------------------------------------------------------------------
-- Forum

data NewThread = NewThread
  { newThread_unit :: Id Unit
  , newThread_author :: Id Person
  , newThread_subject :: Text
  , newThread_message :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON NewThread

data Thread = Thread
  { thread_unit :: Id Unit
  , thread_person :: Id Unit
  , thread_subject :: Text
  , thread_createdAt :: UTCTime
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Thread

data ThreadSummary = ThreadSummary
  { threadSummary_id :: Id Thread
  , threadSummary_unit :: Id Unit
  , threadSummary_author :: Id Unit
  , threadSummary_authorName :: Text
  , threadSummary_subject :: Text
  , threadSummary_createdAt :: UTCTime
  , threadSummary_last :: UTCTime
  , threadSummary_messages :: Int
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON ThreadSummary

data ThreadInfo = ThreadInfo
  { threadInfo_summary :: ThreadSummary
  , threadInfo_messages :: [WithId ThreadMessage]
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON ThreadInfo

data ThreadMessage = ThreadMessage
  { threadMessage_author :: Id Person
  , threadMessage_authorName :: Text
  , threadMessage_message :: Text
  , threadMessage_createdAt :: UTCTime
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON ThreadMessage
