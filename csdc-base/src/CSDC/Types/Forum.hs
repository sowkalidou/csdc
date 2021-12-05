{-# LANGUAGE StrictData #-}

module CSDC.Types.Forum
  ( -- Thread
    Thread (..)
  , NewThread (..)
  , ThreadInfo (..)
    -- Post
  , Post (..)
  , NewPost (..)
  , PostInfo (..)
  ) where

import CSDC.Aeson (JSON (..))
import CSDC.Types.DAO
import CSDC.Types.Id

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Forum

data NewThread = NewThread
  { newThread_subject :: Text
  , newThread_text :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON NewThread

data Thread = Thread
  { thread_unit :: Id Unit
  , thread_author :: Id Person
  , thread_subject :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Thread

data ThreadInfo = ThreadInfo
  { threadInfo_id :: Id Thread
  , threadInfo_unit :: Id Unit
  , threadInfo_author :: Id Person
  , threadInfo_authorName :: Text
  , threadInfo_subject :: Text
  , threadInfo_createdAt :: UTCTime
  , threadInfo_last :: UTCTime
  , threadInfo_messages :: Int
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON ThreadInfo

data NewPost = NewPost
  { newPost_text :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON NewPost

data Post = Post
  { post_thread :: Id Thread
  , post_author :: Id Person
  , post_text :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Post

data PostInfo = PostInfo
  { postInfo_id :: Id Post
  , postInfo_author :: Id Person
  , postInfo_authorName :: Text
  , postInfo_authorImage :: Text
  , postInfo_text :: Text
  , postInfo_createdAt :: UTCTime
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON PostInfo
