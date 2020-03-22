module CSDC.Network.Class
  ( MonadNetwork (..)
  ) where

import CSDC.Data.Id (Id)
import CSDC.Data.IdMap (IdMap)
import CSDC.Network.Types (Person, Unit, Member, Subpart)

class Monad m => MonadNetwork m where

  -- Person manipulation

  selectPerson :: Id Person -> m (Maybe Person)

  insertPerson :: Person -> m (Id Person)

  updatePerson :: Id Person -> Person -> m ()

  deletePerson :: Id Person -> m ()

  -- Unit manipulation

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
