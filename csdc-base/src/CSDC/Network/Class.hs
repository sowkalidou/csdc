{-# LANGUAGE LambdaCase #-}

module CSDC.Network.Class
  ( MonadNetwork (..)
  , checkPerson
  ) where

import CSDC.Data.Id (Id)
import CSDC.Data.IdMap (IdMap)
import CSDC.Network.Types (Person (..), Unit, Member, Subpart)

import qualified CSDC.ORCID as ORCID

import Control.Monad (void)

--------------------------------------------------------------------------------
-- Class

class Monad m => MonadNetwork m where

  -- Person manipulation

  selectPerson :: Id Person -> m (Maybe Person)

  selectPersonORCID :: ORCID.Id -> m (Maybe Person)

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

  selectMemberPerson :: Id Person -> m (IdMap Member)

  selectMemberUnit :: Id Unit -> m (IdMap Member)

  insertMember :: Member -> m (Id Member)

  deleteMember :: Id Member -> m ()

  -- Subpart manipulation

  selectSubpartChild :: Id Unit -> m (IdMap Subpart)

  selectSubpartParent :: Id Unit -> m (IdMap Subpart)

  insertSubpart :: Subpart -> m (Id Subpart)

  deleteSubpart :: Id Subpart -> m ()

checkPerson :: MonadNetwork m => ORCID.Token -> m ()
checkPerson token =
  selectPersonORCID (ORCID.token_orcid token) >>= \case
    Nothing ->
      let
        person = Person
          { person_name = ORCID.token_name token
          , person_orcid = ORCID.token_orcid token
          }
      in
        void $ insertPerson person
    Just _ ->
      pure ()
