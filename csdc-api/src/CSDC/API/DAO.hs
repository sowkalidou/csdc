module CSDC.API.DAO
  ( Server
  , API
  , serveAPI
  ) where

import CSDC.DAO
import CSDC.Prelude hiding (JSON)

import Servant hiding (Server)

--------------------------------------------------------------------------------
-- Synonyms

type Server token api = ServerT api (Action token)
type GetJSON a = Get '[JSON] a
type PostJSON a b = ReqBody '[JSON] a :> Post '[JSON] b
type DeleteJSON a = Delete '[JSON] a
type CaptureId a = Capture "id" (Id a)

--------------------------------------------------------------------------------
-- User API

type UserAPI = GetJSON UserId

serveUserAPI :: Server UserToken UserAPI
serveUserAPI = getUser

--------------------------------------------------------------------------------
-- Person API

type PersonAPI =
       CaptureId Person :> GetJSON (Maybe Person)
  :<|> PostJSON Person (Id Person)
  :<|> CaptureId Person :> PostJSON Person ()
  :<|> CaptureId Person :> DeleteJSON ()
  :<|> CaptureId Person :> "info" :> GetJSON (Maybe PersonInfo)
  :<|> CaptureId Person :> "units" :> GetJSON (IdMap Member Unit)

servePersonAPI :: Server token PersonAPI
servePersonAPI =
       selectPerson
  :<|> insertPerson
  :<|> updatePerson
  :<|> deletePerson
  :<|> getPersonInfo
  :<|> getUserUnits

--------------------------------------------------------------------------------
-- Unit API

type UnitAPI =
       "root" :> Get '[JSON] (Id Unit)
  :<|> CaptureId Unit :> GetJSON (Maybe Unit)
  :<|> PostJSON Unit (Id Unit)
  :<|> CaptureId Unit :> PostJSON Unit ()
  :<|> CaptureId Unit :> DeleteJSON ()
  :<|> CaptureId Unit :> "info" :> GetJSON (Maybe UnitInfo)
  :<|> CaptureId Unit :> "members" :> GetJSON (IdMap Member (WithId Person))
  :<|> CaptureId Unit :> "children" :> GetJSON (IdMap Subpart (WithId Unit))
  :<|> CaptureId Unit :> "parents" :> GetJSON (IdMap Subpart (WithId Unit))
  :<|> "create" :> PostJSON Unit (Id Unit)

serveUnitAPI :: Server token UnitAPI
serveUnitAPI =
       rootUnit
  :<|> selectUnit
  :<|> insertUnit
  :<|> updateUnit
  :<|> deleteUnit
  :<|> getUnitInfo
  :<|> getUnitMembers
  :<|> getUnitChildren
  :<|> getUnitParents
  :<|> createUnit

--------------------------------------------------------------------------------
-- Member API

type MemberAPI =
       "person" :> CaptureId Person :> GetJSON (IdMap Member Member)
  :<|> "unit" :> CaptureId Unit :> GetJSON (IdMap Member Member)
  :<|> PostJSON Member (Id Member)
  :<|> CaptureId Member :> DeleteJSON ()

serveMemberAPI :: Server token MemberAPI
serveMemberAPI =
       selectMembersByPerson
  :<|> selectMembersByUnit
  :<|> insertMember
  :<|> deleteMember

--------------------------------------------------------------------------------
-- Subpart API

type SubpartAPI =
       "child" :> CaptureId Unit :> GetJSON (IdMap Subpart Subpart)
  :<|> "parent" :> CaptureId Unit :> GetJSON (IdMap Subpart Subpart)
  :<|> PostJSON Subpart (Id Subpart)
  :<|> CaptureId Subpart :> DeleteJSON ()

serveSubpartAPI :: Server token SubpartAPI
serveSubpartAPI =
       selectSubpartsByChild
  :<|> selectSubpartsByParent
  :<|> insertSubpart
  :<|> deleteSubpart

--------------------------------------------------------------------------------
-- Message API

type MessageMemberAPI =
       "send" :> PostJSON (Message Member) (Id (Message Member))
  :<|> "reply" :> PostJSON (Reply Member) (Id (Reply Member))
  :<|> "view" :> PostJSON (Id (Reply Member)) ()

serveMessageMemberAPI :: Server token MessageMemberAPI
serveMessageMemberAPI =
       sendMessageMember
  :<|> sendReplyMember
  :<|> viewReplyMember

type MessageSubpartAPI =
       "send" :> PostJSON (Message Subpart) (Id (Message Subpart))
  :<|> "reply" :> PostJSON (Reply Subpart) (Id (Reply Subpart))
  :<|> "view" :> PostJSON (Id (Reply Subpart)) ()

serveMessageSubpartAPI :: Server token MessageSubpartAPI
serveMessageSubpartAPI =
       sendMessageSubpart
  :<|> sendReplySubpart
  :<|> viewReplySubpart

type MessageAPI =
       "member" :> MessageMemberAPI
  :<|> "subpart" :> MessageSubpartAPI
  :<|> "inbox" :> "person" :> CaptureId Person :> GetJSON Inbox
  :<|> "inbox" :> "unit" :> CaptureId Unit :> GetJSON Inbox

serveMessageAPI :: Server token MessageAPI
serveMessageAPI =
       serveMessageMemberAPI
  :<|> serveMessageSubpartAPI
  :<|> inboxPerson
  :<|> inboxUnit

--------------------------------------------------------------------------------
-- API

type API =
       "user" :> UserAPI
  :<|> "person" :> PersonAPI
  :<|> "unit" :> UnitAPI
  :<|> "member" :> MemberAPI
  :<|> "subpart" :> SubpartAPI
  :<|> "message" :> MessageAPI

serveAPI :: Server UserToken API
serveAPI =
       serveUserAPI
  :<|> servePersonAPI
  :<|> serveUnitAPI
  :<|> serveMemberAPI
  :<|> serveSubpartAPI
  :<|> serveMessageAPI
