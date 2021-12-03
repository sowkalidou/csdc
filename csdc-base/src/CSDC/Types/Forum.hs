{-# LANGUAGE StrictData #-}

module CSDC.Types.Forum
  ( Thread (..)
  , NewThread (..)
  , ThreadSummary (..)
  , ThreadInfo (..)
  , ThreadMessage (..)
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
