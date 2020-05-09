module CSDC.API.DAO
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
  :<|> "person" :> Capture "id" (Id Person) :> "units" :> Get '[JSON] (IdMap Member Unit)
  :<|> CRUD "person" Person

servePersonAPI :: (HasUser m, HasDAO m) => ServerT PersonAPI m
servePersonAPI =
       getUser
  :<|> getUserUnits
  :<|> selectPerson
  :<|> insertPerson
  :<|> updatePerson
  :<|> deletePerson

type UnitAPI =
       "unit" :> "root" :> Get '[JSON] (Id Unit)
  :<|> "unit" :> Capture "id" (Id Unit) :> "members" :> Get '[JSON] (IdMap Member (WithId Person))
  :<|> "unit" :> Capture "id" (Id Unit) :> "subparts" :> Get '[JSON] (IdMap Subpart (WithId Unit))
  :<|> CRUD "unit" Unit

serveUnitAPI :: HasDAO m => ServerT UnitAPI m
serveUnitAPI =
       rootUnit
  :<|> getUnitMembers
  :<|> getUnitSubparts
  :<|> selectUnit
  :<|> insertUnit
  :<|> updateUnit
  :<|> deleteUnit

--------------------------------------------------------------------------------
-- API for relations

type REL (name :: Symbol) (left :: Symbol) (right :: Symbol) r a b =
       name :> left :> Capture left (Id a) :> Get '[JSON] (IdMap r r)
  :<|> name :> right :> Capture right (Id b) :> Get '[JSON] (IdMap r r)
  :<|> name :> ReqBody '[JSON] r :> Post '[JSON] (Id r)
  :<|> name :> Capture "id" (Id r) :> Delete '[JSON] ()

type MemberAPI = REL "member" "person" "unit" Member Person Unit

serveMemberAPI :: HasDAO m => ServerT MemberAPI m
serveMemberAPI =
       selectMemberPerson
  :<|> selectMemberUnit
  :<|> insertMember
  :<|> deleteMember

type SubpartAPI = REL "subpart" "child" "parent" Subpart Unit Unit

serveSubpartAPI :: HasDAO m => ServerT SubpartAPI m
serveSubpartAPI =
       selectSubpartChild
  :<|> selectSubpartParent
  :<|> insertSubpart
  :<|> deleteSubpart

--------------------------------------------------------------------------------
-- API

type API = PersonAPI :<|> UnitAPI :<|> MemberAPI :<|> SubpartAPI

serveAPI :: (HasUser m, HasDAO m) => ServerT API m
serveAPI =
       servePersonAPI
  :<|> serveUnitAPI
  :<|> serveMemberAPI
  :<|> serveSubpartAPI
