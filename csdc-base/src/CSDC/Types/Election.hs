module CSDC.Types.Election where

import CSDC.Types.DAO (Person, Unit)
import CSDC.Types.Id (Id)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time (UTCTime)

--------------------------------------------------------------------------------
-- Election

data ElectionType = MajorityConsensus | SimpleMajority
  deriving (Show, Eq)

newtype ElectionChoice = ElectionChoice {getElectionChoice :: Text}
  deriving (Show, Eq)

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
  deriving (Show, Eq)

-- | Used by the UI to create an election
data NewElection = NewElection
  { unitId :: Id Unit,
    title :: Text,
    description :: Text,
    choices :: [ElectionChoice],
    electionType :: ElectionType,
    visibleVotes :: Bool,
    endingAt :: UTCTime
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Votes

data Vote = Vote
  { id :: Id Vote,
    electionId :: Id Election,
    payload :: VotePayload
  }
  deriving (Show, Eq)

data VotePayload
  = VotePayloadMajorityConsensus (HashMap ElectionChoice Grade)
  | VotePayloadSimpleMajority (Maybe ElectionChoice)
  deriving (Show, Eq)

data Grade
  = GradeExcellent
  | GradeVeryGood
  | GradeGood
  | GradeAcceptable
  | GradeBad
  | GradeVeryBad
  deriving (Show, Eq)

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
    personId :: Id Person,
    payload :: VotePayload
  }
  deriving (Show, Eq)
