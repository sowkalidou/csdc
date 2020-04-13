{-# LANGUAGE TemplateHaskell #-}

module CSDC.Network.Mock
  ( Mock
  , runMock
  , Store
  , makeEmptyStore
  ) where

import CSDC.Data.Id (Id (..))
import CSDC.Data.IdMap (IdMap)
import CSDC.Data.RIO (RIO, runRIO)
import CSDC.Network.Types (Person (..), Unit (..), Member (..), Subpart (..))
import CSDC.Network.Class (MonadNetwork (..))

import qualified CSDC.Data.IdMap as IdMap

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Lens (Lens', makeLenses, view, set, use, modifying)

--------------------------------------------------------------------------------
-- In-memory store

data Store = Store
  { _store_person :: IdMap Person
  , _store_unit :: IdMap Unit
  , _store_member :: IdMap Member
  , _store_subpart :: IdMap Subpart
  , _store_root :: Id Unit
  } deriving (Show, Eq)

makeLenses ''Store

makeEmptyStore :: MonadIO m => m (MVar Store)
makeEmptyStore = liftIO $ newMVar
  Store
    { _store_person = IdMap.empty
    , _store_unit = IdMap.insert uid unit IdMap.empty
    , _store_member = IdMap.empty
    , _store_subpart = IdMap.empty
    , _store_root = uid
    }
  where
    uid = Id 0
    unit = Unit "CS-DC"

--------------------------------------------------------------------------------
-- Mock implementation

newtype Mock m a = Mock (RIO Store m a)
  deriving newtype (Functor, Applicative, Monad, MonadState Store)

runMock :: MonadIO m => MVar Store -> Mock m a -> m a
runMock var (Mock m) = runRIO var m

instance MonadIO m => MonadNetwork (Mock m) where

  -- Person manipulation

  selectPerson uid =
    IdMap.lookup uid <$> use store_person

  insertPerson p =
    stating store_person (IdMap.insertNew p)

  updatePerson uid p =
    modifying store_person (IdMap.insert uid p)

  deletePerson uid =
    modifying store_person (IdMap.delete uid)

  -- Unit manipulation
  rootUnit =
    use store_root

  selectUnit uid =
    IdMap.lookup uid <$> use store_unit

  insertUnit u =
    stating store_unit (IdMap.insertNew u)

  updateUnit uid u =
    modifying store_unit (IdMap.insert uid u)

  deleteUnit uid =
    modifying store_unit (IdMap.delete uid)

  -- Member manipulation

  selectMemberPerson uid = do
    let cond (Member u _) = u == uid
    IdMap.filter cond <$> use store_member

  selectMemberUnit uid = do
    let cond (Member _ u) = u == uid
    IdMap.filter cond <$> use store_member

  insertMember m =
    stating store_member (IdMap.insertNew m)

  deleteMember uid =
    modifying store_member (IdMap.delete uid)

  -- Subpart manipulation

  selectSubpartChild uid = do
    let cond (Subpart u _) = u == uid
    IdMap.filter cond <$> use store_subpart

  selectSubpartParent uid = do
    let cond (Subpart _ u) = u == uid
    IdMap.filter cond <$> use store_subpart

  insertSubpart s =
    stating store_subpart (IdMap.insertNew s)

  deleteSubpart uid =
    modifying store_subpart (IdMap.delete uid)

--------------------------------------------------------------------------------
-- Helper

stating :: MonadState s m => Lens' s a -> (a -> (x,a)) -> m x
stating l f = do
  let f' s =
        let (x, a) = f (view l s)
        in (x, set l a s)
  state f'
{-# INLINE stating #-}
