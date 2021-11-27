{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module CSDC.DAO where

import CSDC.Auth.User (User (..))
import CSDC.Prelude

import qualified CSDC.Auth.ORCID as ORCID
import qualified CSDC.SQL as SQL
import qualified CSDC.SQL.Members as SQL.Members
import qualified CSDC.SQL.MessageMembers as SQL.MessageMembers
import qualified CSDC.SQL.MessageSubparts as SQL.MessageSubparts
import qualified CSDC.SQL.Persons as SQL.Persons
import qualified CSDC.SQL.Subparts as SQL.Subparts
import qualified CSDC.SQL.Units as SQL.Units

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), MonadReader (..), asks)
import Data.Time (getCurrentTime)

--------------------------------------------------------------------------------
-- Context

data Context user = Context
  { context_sql :: SQL.Context
  , context_user :: user
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (Context user)

--------------------------------------------------------------------------------
-- Error

data Error
  = ErrorSQL SQL.Error
    deriving (Show, Eq)

instance Exception Error

--------------------------------------------------------------------------------
-- Action

newtype Action user a = Action (ReaderT (Context user) IO a)
  deriving (Functor, Applicative, Monad, MonadReader (Context user), MonadIO)

-- Actions with authentication needed
type ActionAuth = Action (Id Person)

run :: MonadIO m => Context user -> Action user a -> m a
run ctx (Action act) = liftIO $
  runReaderT act ctx

withToken :: UserToken -> ActionAuth a -> Action user a
withToken token (Action (ReaderT act)) = do
  pid <- getUserFromToken token
  Action $ ReaderT $ \ctx -> act $ ctx { context_user = pid }

withPerson :: Id Person -> ActionAuth a -> Action user a
withPerson pid (Action (ReaderT act)) =
  Action $ ReaderT $ \ctx -> act $ ctx { context_user = pid }

runSQL :: SQL.Action a -> Action user a
runSQL act = do
  ctx <- asks context_sql
  SQL.run ctx act >>= \case
    Left e ->
      liftIO $ throwIO $ ErrorSQL e
    Right a ->
      pure a

--------------------------------------------------------------------------------
-- User

getUserFromToken :: UserToken -> Action user (Id Person)
getUserFromToken (User token) =
  selectPersonORCID (ORCID.token_orcid token) >>= \case
    Nothing ->
      let
        person = NewPerson
          { newPerson_name = ORCID.token_name token
          , newPerson_orcid = ORCID.token_orcid token
          , newPerson_description = ""
          }
      in
        insertPerson person
    Just uid ->
      pure uid

getUser :: ActionAuth (Id Person)
getUser = asks context_user

--------------------------------------------------------------------------------
-- Person

selectPerson :: Id Person -> Action user (Maybe Person)
selectPerson i = runSQL $ SQL.query SQL.Persons.select i

insertPerson :: NewPerson -> Action user (Id (Person))
insertPerson p = runSQL $ SQL.query SQL.Persons.insert p

updatePerson :: Id Person -> PersonUpdate -> Action user ()
updatePerson i p = runSQL $ SQL.query SQL.Persons.update (i,p)

deletePerson :: Id Person -> Action user ()
deletePerson i = runSQL $ SQL.query SQL.Persons.delete i

--------------------------------------------------------------------------------
-- Unit

selectUnit :: Id Unit -> Action user (Maybe Unit)
selectUnit i = runSQL $ SQL.query SQL.Units.select i

insertUnit :: NewUnit -> ActionAuth (Id (Unit))
insertUnit u = do
  user <- getUser
  now <- liftIO getCurrentTime
  let unit = Unit
        { unit_name = newUnit_name u
        , unit_description = newUnit_description u
        , unit_chair = user
        , unit_createdAt = now -- will be ignored
        }
  runSQL $ SQL.query SQL.Units.insert unit

updateUnit :: Id Unit -> UnitUpdate -> Action user ()
updateUnit i p = runSQL $ SQL.query SQL.Units.update (i,p)

deleteUnit :: Id Unit -> Action user ()
deleteUnit i = runSQL $ do
  SQL.query SQL.Subparts.deleteUnit i
  SQL.query SQL.Members.deleteUnit i
  SQL.query SQL.Units.delete i

--------------------------------------------------------------------------------
-- Member

insertMember :: NewMember -> Action user (Id Member)
insertMember r = runSQL $ SQL.query SQL.Members.insert r

deleteMember :: Id Member -> Action user ()
deleteMember i = runSQL $ SQL.query SQL.Members.delete i

--------------------------------------------------------------------------------
-- Subpart

insertSubpart :: NewSubpart -> Action user (Id Subpart)
insertSubpart r = runSQL $ SQL.query SQL.Subparts.insert r

deleteSubpart :: Id Subpart -> Action user ()
deleteSubpart i = runSQL $ SQL.query SQL.Subparts.delete i

--------------------------------------------------------------------------------
-- Message NewMember

sendMessageMember :: NewMessage NewMember -> Action user (Id (Message NewMember))
sendMessageMember m = runSQL $ SQL.query SQL.MessageMembers.sendMessage m

sendReplyMember :: NewReply NewMember -> Action user (Id (Reply NewMember))
sendReplyMember r = do
  rid <- runSQL $ SQL.query SQL.MessageMembers.sendReply r
  let
    status = case newReply_type r of
      Accept -> Accepted
      Reject -> Rejected
    uid = newReply_message r
  runSQL $ SQL.query SQL.MessageMembers.updateMessage (uid, status)
  case newReply_type r of
    Accept ->
      runSQL (SQL.query SQL.MessageMembers.selectMember uid) >>= \case
        Nothing ->
          pure ()
        Just val -> do
          _ <- insertMember val
          pure ()
    _ ->
      pure ()
  pure rid

viewReplyMember :: Id (Reply NewMember) -> Action user ()
viewReplyMember i = runSQL $ SQL.query SQL.MessageMembers.viewReply i

--------------------------------------------------------------------------------
-- Message NewSubpart

sendMessageSubpart :: NewMessage NewSubpart -> Action user (Id (Message NewSubpart))
sendMessageSubpart m = runSQL $ SQL.query SQL.MessageSubparts.sendMessage m

sendReplySubpart :: NewReply NewSubpart -> Action user (Id (Reply NewSubpart))
sendReplySubpart r = do
  rid <- runSQL $ SQL.query SQL.MessageSubparts.sendReply r
  let
    status = case newReply_type r of
      Accept -> Accepted
      Reject -> Rejected
    uid = newReply_message r
  runSQL $ SQL.query SQL.MessageSubparts.updateMessage (uid, status)
  case newReply_type r of
    Accept ->
      runSQL (SQL.query SQL.MessageSubparts.selectSubpart uid) >>= \case
        Nothing ->
          pure ()
        Just val -> do
          _ <- insertSubpart val
          pure ()
    _ ->
      pure ()
  pure rid

viewReplySubpart :: Id (Reply NewSubpart) -> Action user ()
viewReplySubpart i = runSQL $ SQL.query SQL.MessageSubparts.viewReply i

--------------------------------------------------------------------------------
-- Others

selectPersonORCID :: ORCID.Id -> Action user (Maybe (Id Person))
selectPersonORCID i = runSQL $ SQL.query SQL.Persons.selectORCID i

createUnit :: NewUnit -> ActionAuth (Id Unit)
createUnit unit = do
  pid <- getUser
  uid <- insertUnit unit
  let member = NewMember
        { newMember_person = pid
        , newMember_unit = uid
        }
  _ <- insertMember member
  return uid

inboxPerson :: Id Person -> Action user Inbox
inboxPerson pid = do
  allMessageMembers <- runSQL $
    SQL.query SQL.MessageMembers.select $
    SQL.MessageMembers.Filter (Just pid) Nothing

  allReplyMembers <- runSQL $
    SQL.query SQL.MessageMembers.messageReplies $ fmap messageInfo_id allMessageMembers

  let
    predMessage m =
      messageInfo_status m == Waiting &&
      messageInfo_type m == Invitation

    predReply r =
      replyInfo_status r == NotSeen &&
      replyInfo_mtype r == Submission

  pure Inbox
    { inbox_messageMember = filter predMessage allMessageMembers
    , inbox_replyMember = filter predReply allReplyMembers
    , inbox_messageSubpart = []
    , inbox_replySubpart = []
    }

inboxUnit :: Id Unit -> Action user Inbox
inboxUnit uid = do
  allMessageSubparts <- runSQL $
    SQL.query SQL.MessageSubparts.select uid

  allReplySubparts <- runSQL $
    SQL.query SQL.MessageSubparts.messageReplies $
    fmap messageInfo_id allMessageSubparts

  let
    predMessageSubpart m =
      messageInfo_status m == Waiting

    predReplySubpart r =
      replyInfo_status r == NotSeen

  allMessageMembers <- runSQL $
    SQL.query SQL.MessageMembers.select $
    SQL.MessageMembers.Filter Nothing (Just uid)

  allReplyMembers <- runSQL $
    SQL.query SQL.MessageMembers.messageReplies $
    fmap messageInfo_id allMessageMembers

  let
    predMessageMember m =
      messageInfo_status m == Waiting &&
      messageInfo_type m == Submission

    predReplyMember r =
      replyInfo_status r == NotSeen &&
      replyInfo_mtype r == Invitation

  pure Inbox
    { inbox_messageMember = filter predMessageMember allMessageMembers
    , inbox_replyMember = filter predReplyMember allReplyMembers
    , inbox_messageSubpart = filter predMessageSubpart allMessageSubparts
    , inbox_replySubpart = filter predReplySubpart allReplySubparts
    }

getPersonInfo :: Id Person -> Action user (Maybe PersonInfo)
getPersonInfo uid =
  selectPerson uid >>= \case
    Nothing -> pure Nothing
    Just person -> do
      members <- runSQL $ SQL.query SQL.Members.selectByPerson uid
      pure $ Just PersonInfo
        { personInfo_id = uid
        , personInfo_person = person
        , personInfo_members = members
        }

getUnitMembers :: Id Unit -> Action user [UnitMember]
getUnitMembers uid = runSQL $ SQL.query SQL.Members.selectByUnit uid

getUnitChildren :: Id Unit -> Action user [UnitSubpart]
getUnitChildren uid = runSQL $ SQL.query SQL.Subparts.selectByParent uid

getUnitParents :: Id Unit -> Action user [UnitSubpart]
getUnitParents uid = runSQL $ SQL.query SQL.Subparts.selectByChild uid

getUnitInfo :: Id Unit -> Action user (Maybe UnitInfo)
getUnitInfo uid =
  selectUnit uid >>= \case
    Nothing -> pure Nothing
    Just unit -> do
      members <- getUnitMembers uid
      children <- getUnitChildren uid
      parents <- getUnitParents uid
      pure $ Just UnitInfo
        { unitInfo_id = uid
        , unitInfo_unit = unit
        , unitInfo_members = members
        , unitInfo_children = children
        , unitInfo_parents = parents
        }

getUnitsWhoseChairIsUser :: ActionAuth [WithId Unit]
getUnitsWhoseChairIsUser = do
  uid <- getUser
  runSQL $ SQL.query SQL.Units.selectByChair uid
