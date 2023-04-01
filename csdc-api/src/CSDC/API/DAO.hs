{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CSDC.API.DAO
  ( API,
    serveAPI,
  )
where

import CSDC.Action
import CSDC.DAO
import CSDC.Prelude
import CSDC.Types.File (Base64File, File (..), FileUI)
import Data.ByteString.Lazy qualified as Lazy
import Servant hiding (Post)
import Servant qualified
import Servant.Multipart

--------------------------------------------------------------------------------
-- Synonyms

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

serveUserAPI :: ServerAuth UserAPI
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

servePersonAPI :: ServerAuth PersonAPI
servePersonAPI =
  updatePerson
    :<|> updatePersonImage
    :<|> getPersonInfo

--------------------------------------------------------------------------------
-- Unit API

type UnitAPI =
  PostJSON NewUnit (Id Unit)
    :<|> CaptureId Unit :> GetJSON (Maybe Unit)
    :<|> CaptureId Unit :> PostJSON UnitUpdate ()
    :<|> CaptureId Unit :> DeleteJSON ()
    :<|> CaptureId Unit :> "image" :> PostJSON Base64File ()
    :<|> CaptureId Unit :> "info" :> GetJSON (Maybe UnitInfo)
    :<|> CaptureId Unit :> "children" :> GetJSON [UnitSubpart]
    :<|> CaptureId Unit :> "parents" :> GetJSON [UnitSubpart]
    :<|> CaptureId Unit :> "chair" :> PostJSON (Id Person) ()
    :<|> CaptureId Unit :> "files" :> GetJSON [FileUI]
    :<|> CaptureId Unit :> "files" :> MultipartForm Mem File :> Servant.Post '[JSON] ()
    :<|> CaptureId Unit :> "invitation" :> PostJSON MailInvitation ()

serveUnitAPI :: ServerAuth UnitAPI
serveUnitAPI =
  createUnit
    :<|> selectUnit
    :<|> updateUnit
    :<|> deleteUnit
    :<|> updateUnitImage
    :<|> getUnitInfo
    :<|> getUnitChildren
    :<|> getUnitParents
    :<|> changeUnitChair
    :<|> getUnitFiles
    :<|> insertUnitFile
    :<|> sendMailInvitation

instance FromMultipart Mem File where
  fromMultipart parts = do
    fileData <- lookupFile "file" parts
    pure
      File
        { name = fdFileName fileData,
          contents = Lazy.toStrict $ fdPayload fileData
        }

--------------------------------------------------------------------------------
-- Member API

type MemberAPI =
  PostJSON NewMember (Id Member)
    :<|> CaptureId Member :> DeleteJSON ()

serveMemberAPI :: ServerAuth MemberAPI
serveMemberAPI =
  insertMember
    :<|> deleteMember

--------------------------------------------------------------------------------
-- Subpart API

type SubpartAPI =
  PostJSON NewSubpart (Id Subpart)
    :<|> CaptureId Subpart :> DeleteJSON ()

serveSubpartAPI :: ServerAuth SubpartAPI
serveSubpartAPI =
  insertSubpart
    :<|> deleteSubpart

--------------------------------------------------------------------------------
-- Message API

type MessageMemberAPI =
  "send" :> PostJSON (NewMessage NewMember) (Id (Message NewMember))
    :<|> "reply" :> PostJSON (NewReply NewMember) (Id (Reply NewMember))
    :<|> "view" :> PostJSON (Id (Reply NewMember)) ()

serveMessageMemberAPI :: ServerAuth MessageMemberAPI
serveMessageMemberAPI =
  sendMessageMember
    :<|> sendReplyMember
    :<|> viewReplyMember

type MessageSubpartAPI =
  "send" :> PostJSON (NewMessage NewSubpart) (Id (Message NewSubpart))
    :<|> "reply" :> PostJSON (NewReply NewSubpart) (Id (Reply NewSubpart))
    :<|> "view" :> PostJSON (Id (Reply NewSubpart)) ()

serveMessageSubpartAPI :: ServerAuth MessageSubpartAPI
serveMessageSubpartAPI =
  sendMessageSubpart
    :<|> sendReplySubpart
    :<|> viewReplySubpart

type MessageAPI =
  "member" :> MessageMemberAPI
    :<|> "subpart" :> MessageSubpartAPI
    :<|> "inbox" :> "unit" :> CaptureId Unit :> GetJSON Inbox

serveMessageAPI :: ServerAuth MessageAPI
serveMessageAPI =
  serveMessageMemberAPI
    :<|> serveMessageSubpartAPI
    :<|> getUnitInbox

--------------------------------------------------------------------------------
-- Search

type SearchAPI =
  "units" :> Capture "query" Text :> GetJSON [WithId Unit]
    :<|> "all" :> Capture "query" Text :> GetJSON [SearchResult SearchId]

serveSearchAPI :: ServerAuth SearchAPI
serveSearchAPI =
  searchUnits
    :<|> searchAll

--------------------------------------------------------------------------------
-- Forum

type ForumAPI =
  "unit" :> CaptureId Unit :> PostJSON NewThread (Id Thread)
    :<|> "unit" :> CaptureId Unit :> GetJSON [ThreadInfo]
    :<|> "thread" :> CaptureId Thread :> PostJSON NewPost (Id Post)
    :<|> "thread" :> CaptureId Thread :> GetJSON [PostInfo]

serveForumAPI :: ServerAuth ForumAPI
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

serveAPI :: ServerAuth API
serveAPI =
  serveUserAPI
    :<|> servePersonAPI
    :<|> serveUnitAPI
    :<|> serveMemberAPI
    :<|> serveSubpartAPI
    :<|> serveMessageAPI
    :<|> serveSearchAPI
    :<|> serveForumAPI
