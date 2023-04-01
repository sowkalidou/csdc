{-# LANGUAGE StrictData #-}

module CSDC.Types.DAO
  ( -- Person
    Person (..),
    NewPerson (..),
    PersonUpdate (..),
    -- Units
    Unit (..),
    NewUnit (..),
    UnitUpdate (..),
    -- Member
    Member (..),
    NewMember (..),
    -- Subpart
    Subpart (..),
    NewSubpart (..),
    -- Messages
    Message (..),
    NewMessage (..),
    MessageStatus (..),
    MessageType (..),
    -- Reply
    Reply (..),
    NewReply (..),
    ReplyStatus (..),
    ReplyType (..),
  )
where

import CSDC.Types.Id
import Data.Aeson (FromJSON, ToJSON)
import Data.Password.Bcrypt (Bcrypt, PasswordHash)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Fundamental types

data Person = Person
  { name :: Text,
    description :: Text,
    image :: Text,
    email :: Text,
    createdAt :: POSIXTime
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data NewPerson = NewPerson
  { name :: Text,
    description :: Text,
    image :: Text,
    email :: Text,
    password :: PasswordHash Bcrypt
  }
  deriving (Show, Eq, Generic)

data PersonUpdate = PersonUpdate
  { name :: Text,
    description :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data Unit = Unit
  { name :: Text,
    description :: Text,
    chairId :: Id Person,
    image :: Text,
    createdAt :: POSIXTime
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data NewUnit = NewUnit
  { name :: Text,
    description :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data UnitUpdate = UnitUpdate
  { name :: Text,
    description :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Fundamental relations

data Member = Member
  { personId :: Id Person,
    unitId :: Id Unit,
    createdAt :: POSIXTime
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data NewMember = NewMember
  { personId :: Id Person,
    unitId :: Id Unit
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data Subpart = Subpart
  { childId :: Id Unit,
    parentId :: Id Unit,
    createdAt :: POSIXTime
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data NewSubpart = NewSubpart
  { childId :: Id Unit,
    parentId :: Id Unit
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Messages

data MessageStatus = Waiting | Accepted | Rejected
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)

data MessageType = Invitation | Submission
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)

data Message a = Message
  { messageType :: MessageType,
    text :: Text,
    status :: MessageStatus,
    value :: a,
    createdAt :: POSIXTime
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data NewMessage a = NewMessage
  { messageType :: MessageType,
    text :: Text,
    value :: a
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data ReplyStatus = Seen | NotSeen
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)

data ReplyType = Accept | Reject
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)

data Reply a = Reply
  { replyType :: ReplyType,
    messageType :: MessageType,
    text :: Text,
    status :: ReplyStatus,
    messageId :: Id (Message a),
    createdAt :: POSIXTime
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data NewReply a = NewReply
  { replyType :: ReplyType,
    text :: Text,
    messageId :: Id (Message a)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
