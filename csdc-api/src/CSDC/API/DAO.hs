module CSDC.API.DAO
  ( Server
  , API
  , serveAPI
  ) where

import CSDC.Types.File (Base64File)
import CSDC.DAO
import CSDC.Prelude hiding (JSON)

import Servant hiding (Server, Post)
import qualified Servant

--------------------------------------------------------------------------------
-- Synonyms

type Server api = ServerT api (Action (Id Person))
type GetJSON a = Get '[JSON] a
type PostJSON a b = ReqBody '[JSON] a :> Servant.Post '[JSON] b
type DeleteJSON a = Delete '[JSON] a
type CaptureId a = Capture "id" (Id a)

--------------------------------------------------------------------------------
-- User API

type UserAPI =
       "info" :> GetJSON (Maybe PersonInfo)
  :<|> "inbox" :> GetJSON Inbox
  :<|> "units" :> GetJSON [WithId Unit]

serveUserAPI :: Server UserAPI
serveUserAPI =
       getUserInfo
  :<|> getUserInbox
  :<|> getUnitsWhoseChairIsUser

--------------------------------------------------------------------------------
-- Person API

type PersonAPI =
       CaptureId Person :> PostJSON PersonUpdate ()
  :<|> CaptureId Person :> "image" :> PostJSON Base64File ()
  :<|> CaptureId Person :> "info" :> GetJSON (Maybe PersonInfo)

servePersonAPI :: Server PersonAPI
servePersonAPI =
       updatePerson
  :<|> updatePersonImage
  :<|> getPersonInfo

--------------------------------------------------------------------------------
-- Unit API

type UnitAPI =
       CaptureId Unit :> GetJSON (Maybe Unit)
  :<|> CaptureId Unit :> PostJSON UnitUpdate ()
  :<|> CaptureId Unit :> DeleteJSON ()
  :<|> CaptureId Unit :> "image" :> PostJSON Base64File ()
  :<|> CaptureId Unit :> "info" :> GetJSON (Maybe UnitInfo)
  :<|> CaptureId Unit :> "children" :> GetJSON [UnitSubpart]
  :<|> CaptureId Unit :> "parents" :> GetJSON [UnitSubpart]
  :<|> PostJSON NewUnit (Id Unit)

serveUnitAPI :: Server UnitAPI
serveUnitAPI =
       selectUnit
  :<|> updateUnit
  :<|> deleteUnit
  :<|> updateUnitImage
  :<|> getUnitInfo
  :<|> getUnitChildren
  :<|> getUnitParents
  :<|> createUnit

--------------------------------------------------------------------------------
-- Member API

type MemberAPI =
       PostJSON NewMember (Id Member)
  :<|> CaptureId Member :> DeleteJSON ()

serveMemberAPI :: Server MemberAPI
serveMemberAPI =
       insertMember
  :<|> deleteMember

--------------------------------------------------------------------------------
-- Subpart API

type SubpartAPI =
       PostJSON NewSubpart (Id Subpart)
  :<|> CaptureId Subpart :> DeleteJSON ()

serveSubpartAPI :: Server SubpartAPI
serveSubpartAPI =
       insertSubpart
  :<|> deleteSubpart

--------------------------------------------------------------------------------
-- Message API

type MessageMemberAPI =
       "send" :> PostJSON (NewMessage NewMember) (Id (Message NewMember))
  :<|> "reply" :> PostJSON (NewReply NewMember) (Id (Reply NewMember))
  :<|> "view" :> PostJSON (Id (Reply NewMember)) ()

serveMessageMemberAPI :: Server MessageMemberAPI
serveMessageMemberAPI =
       sendMessageMember
  :<|> sendReplyMember
  :<|> viewReplyMember

type MessageSubpartAPI =
       "send" :> PostJSON (NewMessage NewSubpart) (Id (Message NewSubpart))
  :<|> "reply" :> PostJSON (NewReply NewSubpart) (Id (Reply NewSubpart))
  :<|> "view" :> PostJSON (Id (Reply NewSubpart)) ()

serveMessageSubpartAPI :: Server MessageSubpartAPI
serveMessageSubpartAPI =
       sendMessageSubpart
  :<|> sendReplySubpart
  :<|> viewReplySubpart

type MessageAPI =
       "member" :> MessageMemberAPI
  :<|> "subpart" :> MessageSubpartAPI
  :<|> "inbox" :> "unit" :> CaptureId Unit :> GetJSON Inbox

serveMessageAPI :: Server MessageAPI
serveMessageAPI =
       serveMessageMemberAPI
  :<|> serveMessageSubpartAPI
  :<|> getUnitInbox

--------------------------------------------------------------------------------
-- Search

type SearchAPI =
       "units" :> Capture "query" Text :> GetJSON [WithId Unit]
  :<|> "all":> Capture "query" Text :> GetJSON [SearchResult SearchId]

serveSearchAPI :: Server SearchAPI
serveSearchAPI =
       searchUnits
  :<|> searchAll

--------------------------------------------------------------------------------
-- Forum

type ForumAPI =
       "unit" :> CaptureId Unit :> PostJSON NewThread (Id Thread)
  :<|> "unit" :> CaptureId Unit :> GetJSON [ThreadInfo]
  :<|> "threads" :> CaptureId Thread :> PostJSON NewPost (Id Post)
  :<|> "threads" :> CaptureId Thread :> GetJSON [PostInfo]

serveForumAPI :: Server ForumAPI
serveForumAPI =
       createThread
  :<|> getThreads
  :<|> createPost
  :<|> getPosts

--------------------------------------------------------------------------------
-- API

type API =
       "user" :> UserAPI
  :<|> "person" :> PersonAPI
  :<|> "unit" :> UnitAPI
  :<|> "member" :> MemberAPI
  :<|> "subpart" :> SubpartAPI
  :<|> "message" :> MessageAPI
  :<|> "search" :> SearchAPI
  :<|> "forum" :> ForumAPI

serveAPI :: Server API
serveAPI =
       serveUserAPI
  :<|> servePersonAPI
  :<|> serveUnitAPI
  :<|> serveMemberAPI
  :<|> serveSubpartAPI
  :<|> serveMessageAPI
  :<|> serveSearchAPI
  :<|> serveForumAPI
