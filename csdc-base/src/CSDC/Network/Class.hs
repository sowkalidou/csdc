{-# LANGUAGE LambdaCase #-}

module CSDC.Network.Class
  ( HasNetwork (..)
  , getUserUnits
  ) where

import CSDC.Data.Id (Id)
import CSDC.Data.IdMap (IdMap)
import CSDC.Network.Types (Person (..), Unit, Member (..), Subpart)

import qualified CSDC.Auth.ORCID as ORCID
import qualified CSDC.Data.IdMap as IdMap

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans (..))
import Data.Traversable (forM)

--------------------------------------------------------------------------------
-- Class

class Monad m => HasNetwork m where

  -- Person manipulation

  selectPerson :: Id Person -> m (Maybe Person)

  selectPersonORCID :: ORCID.Id -> m (Maybe (Id Person))

  insertPerson :: Person -> m (Id Person)

  updatePerson :: Id Person -> Person -> m ()

  deletePerson :: Id Person -> m ()

  -- Unit manipulation

  rootUnit :: m (Id Unit)

  selectUnit :: Id Unit -> m (Maybe Unit)

  insertUnit :: Unit -> m (Id Unit)

  updateUnit :: Id Unit -> Unit -> m ()

  deleteUnit :: Id Unit -> m ()

  -- Member manipulation

  selectMemberPerson :: Id Person -> m (IdMap Member Member)

  selectMemberUnit :: Id Unit -> m (IdMap Member Member)

  insertMember :: Member -> m (Id Member)

  deleteMember :: Id Member -> m ()

  -- Subpart manipulation

  selectSubpartChild :: Id Unit -> m (IdMap Subpart Subpart)

  selectSubpartParent :: Id Unit -> m (IdMap Subpart Subpart)

  insertSubpart :: Subpart -> m (Id Subpart)

  deleteSubpart :: Id Subpart -> m ()

getUserUnits :: HasNetwork m => Id Person -> m (IdMap Member Unit)
getUserUnits uid = do
  members <- selectMemberPerson uid
  pairs <- forM members $ \(Member _ unitId) ->
    selectUnit unitId
  case sequence pairs of
    Nothing -> pure IdMap.empty
    Just m -> pure m

--------------------------------------------------------------------------------
-- Instances

-- | This instance is here for the delegation to @UserT@. It only depends on
-- 'MonadTrans'.
instance HasNetwork m => HasNetwork (ReaderT r m) where
  selectPerson  = lift1 selectPerson
  selectPersonORCID = lift1 selectPersonORCID
  insertPerson = lift1 insertPerson
  updatePerson = lift2 updatePerson
  deletePerson = lift1 deletePerson
  rootUnit = lift rootUnit
  selectUnit = lift1 selectUnit
  insertUnit = lift1 insertUnit
  updateUnit = lift2 updateUnit
  deleteUnit = lift1 deleteUnit
  selectMemberPerson = lift1 selectMemberPerson
  selectMemberUnit = lift1 selectMemberUnit
  insertMember = lift1 insertMember
  deleteMember = lift1 deleteMember
  selectSubpartChild = lift1 selectSubpartChild
  selectSubpartParent = lift1 selectSubpartParent
  insertSubpart = lift1 insertSubpart
  deleteSubpart = lift1 deleteSubpart

lift1 :: (MonadTrans t, Monad m) => (a -> m b) -> a -> t m b
lift1 f a = lift (f a)

lift2 :: (MonadTrans t, Monad m) => (a -> b -> m c) -> a -> b -> t m c
lift2 f a b = lift (f a b)
