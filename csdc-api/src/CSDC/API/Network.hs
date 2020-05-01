module CSDC.API.Network
  ( API
  , serveAPI
  ) where

import CSDC.Prelude hiding (JSON)

import GHC.Types (Symbol)
import Servant

--------------------------------------------------------------------------------
-- API for types

type CRUD (name :: Symbol) a =
       name :> Capture "id" (Id a) :> Get '[JSON] (Maybe a)
  :<|> name :> ReqBody '[JSON] a :> Post '[JSON] (Id a)
  :<|> name :> Capture "id" (Id a) :> ReqBody '[JSON] a :> Post '[JSON] ()
  :<|> name :> Capture "id" (Id a) :> Delete '[JSON] ()

type PersonAPI =
       "person" :> "root" :> Get '[JSON] UserId
  :<|> CRUD "person" Person

servePersonAPI :: (HasUser m, HasNetwork m) => ServerT PersonAPI m
servePersonAPI =
       getUser
  :<|> selectPerson
  :<|> insertPerson
  :<|> updatePerson
  :<|> deletePerson

type UnitAPI =
       "unit" :> "root" :> Get '[JSON] (Id Unit)
  :<|> CRUD "unit" Unit

serveUnitAPI :: HasNetwork m => ServerT UnitAPI m
serveUnitAPI =
       rootUnit
  :<|> selectUnit
  :<|> insertUnit
  :<|> updateUnit
  :<|> deleteUnit

--------------------------------------------------------------------------------
-- API for relations

type REL (name :: Symbol) (left :: Symbol) (right :: Symbol) r a b =
       name :> left :> Capture left (Id a) :> Get '[JSON] (IdMap r)
  :<|> name :> right :> Capture right (Id b) :> Get '[JSON] (IdMap r)
  :<|> name :> ReqBody '[JSON] r :> Post '[JSON] (Id r)
  :<|> name :> Capture "id" (Id r) :> Delete '[JSON] ()

type MemberAPI = REL "member" "person" "unit" Member Person Unit

serveMemberAPI :: HasNetwork m => ServerT MemberAPI m
serveMemberAPI =
       selectMemberPerson
  :<|> selectMemberUnit
  :<|> insertMember
  :<|> deleteMember

type SubpartAPI = REL "subpart" "child" "parent" Subpart Unit Unit

serveSubpartAPI :: HasNetwork m => ServerT SubpartAPI m
serveSubpartAPI =
       selectSubpartChild
  :<|> selectSubpartParent
  :<|> insertSubpart
  :<|> deleteSubpart

--------------------------------------------------------------------------------
-- API

type API = PersonAPI :<|> UnitAPI :<|> MemberAPI :<|> SubpartAPI

serveAPI :: (HasUser m, HasNetwork m) => ServerT API m
serveAPI =
       servePersonAPI
  :<|> serveUnitAPI
  :<|> serveMemberAPI
  :<|> serveSubpartAPI
