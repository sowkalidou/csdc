module CSDC.Types.Election where

import CSDC.Types.DAO (Person, Unit)
import CSDC.Types.Id (Id)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Election

data ElectionType = MajorityConsensus | SimpleMajority
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype ElectionChoice = ElectionChoice {getElectionChoice :: Text}
  deriving (Show, Eq)
  deriving newtype (FromJSON, ToJSON, Hashable, FromJSONKey, ToJSONKey)

data Election = Election
  { id :: Id Election,
    unitId :: Id Unit,
    title :: Text,
    description :: Text,
    choices :: [ElectionChoice],
    electionType :: ElectionType,
    visibleVotes :: Bool,
    endingAt :: UTCTime,
    result :: Maybe ElectionChoice,
    resultComputedAt :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ElectionInfo = ElectionInfo
  { election :: Election,
    votedAt :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Used by the UI to create an election
data NewElection = NewElection
  { title :: Text,
    description :: Text,
    choices :: [ElectionChoice],
    electionType :: ElectionType,
    visibleVotes :: Bool,
    endingAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Votes

data Vote = Vote
  { id :: Id Vote,
    electionId :: Id Election,
    payload :: VotePayload
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VotePayload
  = VotePayloadMajorityConsensus (HashMap ElectionChoice Grade)
  | VotePayloadSimpleMajority (Maybe ElectionChoice)
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Grade
  = GradeExcellent
  | GradeVeryGood
  | GradeGood
  | GradeAcceptable
  | GradeBad
  | GradeVeryBad
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Voter

data Voter = Voter
  { id :: Id Voter,
    electionId :: Id Election,
    personId :: Id Person,
    votedAt :: UTCTime,
    voteId :: Maybe (Id Vote)
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- New Vote

data NewVote = NewVote
  { electionId :: Id Election,
    payload :: VotePayload
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
