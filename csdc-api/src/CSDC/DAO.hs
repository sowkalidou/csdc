{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module CSDC.DAO where

import CSDC.Auth.User (User (..))
import CSDC.Prelude

import qualified CSDC.Auth.ORCID as ORCID
import qualified CSDC.Data.IdMap as IdMap
import qualified CSDC.SQL as SQL
import qualified CSDC.SQL.Members as SQL.Members
import qualified CSDC.SQL.MessageMembers as SQL.MessageMembers
import qualified CSDC.SQL.MessageSubparts as SQL.MessageSubparts
import qualified CSDC.SQL.Persons as SQL.Persons
import qualified CSDC.SQL.Subparts as SQL.Subparts
import qualified CSDC.SQL.Units as SQL.Units

import Control.Exception (Exception, throwIO)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), MonadReader (..), asks)

--------------------------------------------------------------------------------
-- Context

data Context token = Context
  { context_sql :: SQL.Context
  , context_token :: token
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON (Context token)

--------------------------------------------------------------------------------
-- Error

data Error
  = ErrorSQL SQL.Error
    deriving (Show, Eq)

instance Exception Error

--------------------------------------------------------------------------------
-- Action

newtype Action token a = Action (ReaderT (Context token) IO a)
  deriving (Functor, Applicative, Monad, MonadReader (Context token), MonadIO)

run :: MonadIO m => Context token -> Action token a -> m a
run ctx (Action act) = liftIO $
  runReaderT act ctx

withToken :: token -> Action token a -> Action () a
withToken token (Action (ReaderT act)) = Action $ ReaderT $ \ctx ->
  act $ ctx { context_token = token }

check :: Action token ()
check = do
  selectUnit rootUnitId >>= \case
    Nothing -> do
      let person = Person "President" "" (ORCID.Id "")
      runSQL $ SQL.query SQL.Persons.insertAt (rootPersonId,person)
      let unit = Unit "CSDC" "" rootPersonId
      runSQL $ SQL.query SQL.Units.insertAt (rootUnitId,unit)
      let member = Member rootPersonId rootUnitId
      _ <- runSQL $ SQL.query SQL.Members.insert member
      pure ()
    Just _ ->
      pure ()

runSQL :: SQL.Action a -> Action token a
runSQL act = do
  ctx <- asks context_sql
  SQL.run ctx act >>= \case
    Left e ->
      liftIO $ throwIO $ ErrorSQL e
    Right a ->
      pure a

--------------------------------------------------------------------------------
-- User

type UserId = User (Id Person)

getUser :: Action UserToken UserId
getUser =
  fmap context_token ask >>= \case
    Admin ->
      pure Admin

    User token ->
      selectPersonORCID (ORCID.token_orcid token) >>= \case
        Nothing ->
          let
            person = Person
              { person_name = ORCID.token_name token
              , person_orcid = ORCID.token_orcid token
              , person_description = ""
              }
          in
            User <$> insertPerson person
        Just uid ->
          pure $ User uid

--------------------------------------------------------------------------------
-- Person

selectPerson :: Id Person -> Action token (Maybe Person)
selectPerson i = runSQL $ SQL.query SQL.Persons.select i

insertPerson :: Person -> Action token (Id (Person))
insertPerson p = runSQL $ SQL.query SQL.Persons.insert p

updatePerson :: Id Person -> Person -> Action token ()
updatePerson i p = runSQL $ SQL.query SQL.Persons.update (i,p)

deletePerson :: Id Person -> Action token ()
deletePerson i = runSQL $ SQL.query SQL.Persons.delete i

--------------------------------------------------------------------------------
-- Unit

selectUnit :: Id Unit -> Action token (Maybe Unit)
selectUnit i = runSQL $ SQL.query SQL.Units.select i

insertUnit :: Unit -> Action token (Id (Unit))
insertUnit p = runSQL $ SQL.query SQL.Units.insert p

updateUnit :: Id Unit -> Unit -> Action token ()
updateUnit i p = runSQL $ SQL.query SQL.Units.update (i,p)

deleteUnit :: Id Unit -> Action token ()
deleteUnit i = runSQL $ do
  SQL.query SQL.Subparts.deleteUnit i
  SQL.query SQL.Members.deleteUnit i
  SQL.query SQL.Units.delete i

--------------------------------------------------------------------------------
-- Member

selectMembersByPerson :: Id Person -> Action token (IdMap Member Member)
selectMembersByPerson i = runSQL $ SQL.query SQL.Members.selectL i

selectMembersByUnit :: Id Unit -> Action token (IdMap Member Member)
selectMembersByUnit i = runSQL $ SQL.query SQL.Members.selectR i

insertMember :: Member -> Action token (Id Member)
insertMember r = runSQL $ SQL.query SQL.Members.insert r

deleteMember :: Id Member -> Action token ()
deleteMember i = runSQL $ SQL.query SQL.Members.delete i

--------------------------------------------------------------------------------
-- Subpart

selectSubpartsByChild :: Id Unit -> Action token (IdMap Subpart Subpart)
selectSubpartsByChild i = runSQL $ SQL.query SQL.Subparts.selectL i

selectSubpartsByParent :: Id Unit -> Action token (IdMap Subpart Subpart)
selectSubpartsByParent i = runSQL $ SQL.query SQL.Subparts.selectR i

insertSubpart :: Subpart -> Action token (Id Subpart)
insertSubpart r = runSQL $ SQL.query SQL.Subparts.insert r

deleteSubpart :: Id Subpart -> Action token ()
deleteSubpart i = runSQL $ SQL.query SQL.Subparts.delete i

--------------------------------------------------------------------------------
-- Message Member

sendMessageMember :: Message Member -> Action token (Id (Message Member))
sendMessageMember m = runSQL $ SQL.query SQL.MessageMembers.sendMessage m

sendReplyMember :: Reply Member -> Action token (Id (Reply Member))
sendReplyMember r = do
  rid <- runSQL $ SQL.query SQL.MessageMembers.sendReply r
  let
    status = case reply_type r of
      Accept -> Accepted
      Reject -> Rejected
    uid = reply_id r
  runSQL $ SQL.query SQL.MessageMembers.updateMessage (uid, status)
  case reply_type r of
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

viewReplyMember :: Id (Reply Member) -> Action token ()
viewReplyMember i = runSQL $ SQL.query SQL.MessageMembers.viewReply i

--------------------------------------------------------------------------------
-- Message Subpart

sendMessageSubpart :: Message Subpart -> Action token (Id (Message Subpart))
sendMessageSubpart m = runSQL $ SQL.query SQL.MessageSubparts.sendMessage m

sendReplySubpart :: Reply Subpart -> Action token (Id (Reply Subpart))
sendReplySubpart r = do
  rid <- runSQL $ SQL.query SQL.MessageSubparts.sendReply r
  let
    status = case reply_type r of
      Accept -> Accepted
      Reject -> Rejected
    uid = reply_id r
  runSQL $ SQL.query SQL.MessageSubparts.updateMessage (uid, status)
  case reply_type r of
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

viewReplySubpart :: Id (Reply Subpart) -> Action token ()
viewReplySubpart i = runSQL $ SQL.query SQL.MessageSubparts.viewReply i

--------------------------------------------------------------------------------
-- Others

selectPersonORCID :: ORCID.Id -> Action token (Maybe (Id Person))
selectPersonORCID i = runSQL $ SQL.query SQL.Persons.selectORCID i

rootUnit :: Action token (Id Unit)
rootUnit = pure rootUnitId

createUnit :: Unit -> Action token (Id Unit)
createUnit unit@(Unit {unit_chair}) = do
  uid <- insertUnit unit
  let member = Member
        { member_person = unit_chair
        , member_unit = uid
        }
  _ <- insertMember member
  return uid

inboxPerson :: Id Person -> Action token Inbox
inboxPerson pid = do
  messagesAll <- runSQL $
    SQL.query SQL.MessageMembers.select $
    SQL.MessageMembers.Filter (Just pid) Nothing
  let mids = fmap fst messagesAll
  repliesAll <- runSQL $
    SQL.query SQL.MessageMembers.messageReplies mids
  let
    predMessage m =
      messageInfo_status m == Waiting &&
      messageInfo_type m == Invitation

    messageMember =
      IdMap.filter predMessage $
      IdMap.fromList messagesAll

    predReply r =
      replyInfo_status r == NotSeen &&
      replyInfo_mtype r == Submission

    replyMember =
      IdMap.filter predReply $
      IdMap.fromList repliesAll

  pure Inbox
    { inbox_messageMember = messageMember
    , inbox_replyMember = replyMember
    , inbox_messageSubpart = IdMap.empty
    , inbox_replySubpart = IdMap.empty
    }

inboxUnit :: Id Unit -> Action token Inbox
inboxUnit uid = do
  messagesSubpartAll <- runSQL $
    SQL.query SQL.MessageSubparts.select uid

  repliesSubpartAll <- runSQL $
    SQL.query SQL.MessageSubparts.messageReplies $
    fmap fst messagesSubpartAll

  let
    predMessageSubpart m =
      messageInfo_status m == Waiting

    messagesSubpart =
      IdMap.filter predMessageSubpart $
      IdMap.fromList messagesSubpartAll

    predReplySubpart r =
      replyInfo_status r == NotSeen

    repliesSubpart =
      IdMap.filter predReplySubpart $
      IdMap.fromList repliesSubpartAll

  messagesMemberAll <- runSQL $
    SQL.query SQL.MessageMembers.select $
    SQL.MessageMembers.Filter Nothing (Just uid)

  repliesMemberAll <- runSQL $
    SQL.query SQL.MessageMembers.messageReplies $
    fmap fst messagesMemberAll

  let
    predMessageMember m =
      messageInfo_status m == Waiting &&
      messageInfo_type m == Submission

    messageMember =
      IdMap.filter predMessageMember $
      IdMap.fromList messagesMemberAll

    predReplyMember r =
      replyInfo_status r == NotSeen &&
      replyInfo_mtype r == Invitation

    replyMember =
      IdMap.filter predReplyMember $
      IdMap.fromList repliesMemberAll

  pure Inbox
    { inbox_messageMember = messageMember
    , inbox_replyMember = replyMember
    , inbox_messageSubpart = messagesSubpart
    , inbox_replySubpart = repliesSubpart
    }

getPersonInfo :: Id Person -> Action token (Maybe PersonInfo)
getPersonInfo uid =
  selectPerson uid >>= \case
    Nothing -> pure Nothing
    Just person -> do
      membersList <- selectMembersByPerson uid
      pairs <- forM membersList $ \(Member _ unitId) ->
        fmap (WithId unitId) <$> selectUnit unitId
      case sequence pairs of
        Nothing ->
          pure Nothing
        Just members ->
          pure $ Just PersonInfo
            { personInfo_id = uid
            , personInfo_person = person
            , personInfo_members = members
            }

getUnitInfo :: Id Unit -> Action token (Maybe UnitInfo)
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

getUserUnits :: Id Person -> Action token (IdMap Member Unit)
getUserUnits uid = do
  members <- selectMembersByPerson uid
  pairs <- forM members $ \(Member _ unitId) ->
    selectUnit unitId
  case sequence pairs of
    Nothing -> pure IdMap.empty
    Just m -> pure m

getUnitMembers :: Id Unit -> Action token (IdMap Member (WithId Person))
getUnitMembers uid = do
  members <- selectMembersByUnit uid
  pairs <- forM members $ \(Member personId _) ->
    fmap (WithId personId) <$> selectPerson personId
  case sequence pairs of
    Nothing -> pure IdMap.empty
    Just m -> pure m

getUnitChildren :: Id Unit -> Action token (IdMap Subpart (WithId Unit))
getUnitChildren uid = do
  subparts <- selectSubpartsByParent uid
  pairs <- forM subparts $ \(Subpart childId _) ->
    fmap (WithId childId) <$> selectUnit childId
  case sequence pairs of
    Nothing -> pure IdMap.empty
    Just m -> pure m

getUnitParents :: Id Unit -> Action token (IdMap Subpart (WithId Unit))
getUnitParents uid = do
  subparts <- selectSubpartsByChild uid
  pairs <- forM subparts $ \(Subpart _ parentId) ->
    fmap (WithId parentId) <$> selectUnit parentId
  case sequence pairs of
    Nothing -> pure IdMap.empty
    Just m -> pure m

--------------------------------------------------------------------------------
-- Helpers

rootUnitId :: Id Unit
rootUnitId = Id 0

rootPersonId :: Id Person
rootPersonId = Id 0
