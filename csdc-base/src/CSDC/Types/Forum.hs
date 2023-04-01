{-# LANGUAGE StrictData #-}

module CSDC.Types.Forum
  ( -- Thread
    Thread (..),
    NewThread (..),
    ThreadInfo (..),
    -- Post
    Post (..),
    NewPost (..),
    PostInfo (..),
  )
where

import CSDC.Types.DAO
import CSDC.Types.Id
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Forum

data NewThread = NewThread
  { subject :: Text,
    text :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data Thread = Thread
  { unitId :: Id Unit,
    authorId :: Id Person,
    subject :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data ThreadInfo = ThreadInfo
  { id :: Id Thread,
    unitId :: Id Unit,
    authorId :: Id Person,
    authorName :: Text,
    subject :: Text,
    createdAt :: POSIXTime,
    last :: POSIXTime,
    messages :: Int
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data NewPost = NewPost
  { text :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data Post = Post
  { threadId :: Id Thread,
    authorId :: Id Person,
    text :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data PostInfo = PostInfo
  { id :: Id Post,
    authorId :: Id Person,
    authorName :: Text,
    authorImage :: Text,
    text :: Text,
    createdAt :: POSIXTime
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
