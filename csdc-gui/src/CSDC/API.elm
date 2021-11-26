module CSDC.API exposing (..)

import Http
import CSDC.Types exposing (..)
import Json.Decode as D

--------------------------------------------------------------------------------
-- Helpers

type alias Response a = Result Http.Error a

baseUrl : String
baseUrl = "/api/"

decodeNull : D.Decoder ()
decodeNull =
  D.map (\_ -> ()) (D.array D.int)

ignoreResult : Result e a -> Result e ()
ignoreResult = Result.map (\_ -> ())

--------------------------------------------------------------------------------
-- Msg

type Msg
  = RootPerson (Response UserId)
  | GetPersonInfo (Response PersonInfo)
  | UnitsPerson (Response (IdMap Member Unit))
  | SelectPerson (Id Person) (Response Person)
  | InsertPerson (Response (Id Person))
  | DeletePerson (Response ())
  | RootUnit (Response (Id Unit))
  | GetUnitInfo (Response UnitInfo)
  | GetUnitMembers (Response (IdMap Member (WithId Person)))
  | GetUnitChildren (Response (IdMap Subpart (WithId Unit)))
  | GetUnitParents (Response (IdMap Subpart (WithId Unit)))
  | CreateUnit (Response (Id Unit))
  | SelectUnit (Id Unit) (Response Unit)
  | DeleteUnit (Response ())
  | SelectMemberPerson (Response (IdMap Member Member))
  | SelectMemberUnit (Response (IdMap Member Member))
  | InsertMember (Response (Id Member))
  | DeleteMember (Response ())
  | SelectSubpartChild (Response (IdMap Subpart Subpart))
  | SelectSubpartParent (Response (IdMap Subpart Subpart))
  | InsertSubpart (Response (Id Subpart))
  | DeleteSubpart (Response ())
  | SendMessageMember (Response (Id (Message Member)))
  | ViewReplyMember (Response ())
  | SendMessageSubpart (Response (Id (Message Subpart)))
  | ViewReplySubpart (Response ())
  | PersonInbox (Response Inbox)
  | UnitInbox (Id Unit) (Response Inbox)

--------------------------------------------------------------------------------
-- Person

rootPerson : Cmd Msg
rootPerson =
  Http.get
    { url = baseUrl ++ "user"
    , expect = Http.expectJson RootPerson decodeId
    }

getPersonInfo : Id Person -> Cmd Msg
getPersonInfo id =
  Http.get
    { url = baseUrl ++ "person/" ++ idToString id ++ "/info"
    , expect = Http.expectJson GetPersonInfo decodePersonInfo
    }

unitsPerson : Id Person -> Cmd Msg
unitsPerson id =
  Http.get
    { url = baseUrl ++ "person/" ++ idToString id ++ "/units"
    , expect = Http.expectJson UnitsPerson (decodeIdMap decodeUnit)
    }

selectPerson : Id Person -> Cmd Msg
selectPerson id =
  Http.get
    { url = baseUrl ++ "person/" ++ idToString id
    , expect = Http.expectJson (SelectPerson id) decodePerson
    }

insertPerson : Person -> Cmd Msg
insertPerson person =
  Http.post
    { url = baseUrl ++ "person"
    , body = Http.jsonBody (encodePerson person)
    , expect = Http.expectJson InsertPerson decodeId
    }

updatePerson : Id Person -> PersonUpdate -> Cmd (Response ())
updatePerson id person =
  Http.post
    { url = baseUrl ++ "person/" ++ idToString id
    , body = Http.jsonBody (encodePersonUpdate person)
    , expect = Http.expectJson identity decodeNull
    }

deletePerson : Id Person -> Cmd Msg
deletePerson = delete "person" DeletePerson

--------------------------------------------------------------------------------
-- Unit

rootUnit : Cmd Msg
rootUnit =
  Http.get
    { url = baseUrl ++ "unit/root"
    , expect = Http.expectJson RootUnit decodeId
    }

getUnitInfo : Id Unit -> Cmd Msg
getUnitInfo id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/info"
    , expect = Http.expectJson GetUnitInfo decodeUnitInfo
    }

getUnitMembers : Id Unit -> Cmd Msg
getUnitMembers id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/members"
    , expect = Http.expectJson GetUnitMembers (decodeIdMap (decodeWithId decodePerson))
    }

getUnitChildren : Id Unit -> Cmd Msg
getUnitChildren id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/children"
    , expect = Http.expectJson GetUnitChildren (decodeIdMap (decodeWithId decodeUnit))
    }

getUnitParents : Id Unit -> Cmd Msg
getUnitParents id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/parents"
    , expect = Http.expectJson GetUnitParents (decodeIdMap (decodeWithId decodeUnit))
    }

createUnit : NewUnit -> Cmd (Response (Id Unit))
createUnit unit =
  Http.post
    { url = baseUrl ++ "unit/create"
    , body = Http.jsonBody (encodeNewUnit unit)
    , expect = Http.expectJson identity decodeId
    }

selectUnit : Id Unit -> Cmd Msg
selectUnit id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id
    , expect = Http.expectJson (SelectUnit id) decodeUnit
    }

updateUnit : Id Unit -> UnitUpdate -> Cmd (Response ())
updateUnit id unit =
  Http.post
    { url = baseUrl ++ "unit/" ++ idToString id
    , body = Http.jsonBody (encodeUnitUpdate unit)
    , expect = Http.expectJson identity decodeNull
    }

deleteUnit : Id Unit -> Cmd Msg
deleteUnit = delete "unit" DeleteUnit

--------------------------------------------------------------------------------
-- Member

selectMemberPerson : Id Person -> Cmd Msg
selectMemberPerson id =
  Http.get
    { url = baseUrl ++ "member/person/" ++ idToString id
    , expect = Http.expectJson SelectMemberPerson (decodeIdMap decodeMember)
    }

selectMemberUnit : Id Unit -> Cmd Msg
selectMemberUnit id =
  Http.get
    { url = baseUrl ++ "member/unit/" ++ idToString id
    , expect = Http.expectJson SelectMemberUnit (decodeIdMap decodeMember)
    }

insertMember : Member -> Cmd Msg
insertMember member =
  Http.post
    { url = baseUrl ++ "member"
    , body = Http.jsonBody (encodeMember member)
    , expect = Http.expectJson InsertMember decodeId
    }

deleteMember : Id Member -> Cmd Msg
deleteMember = delete "member" DeleteMember

--------------------------------------------------------------------------------
-- Subpart

selectSubpartChild : Id Unit -> Cmd Msg
selectSubpartChild id =
  Http.get
    { url = baseUrl ++ "subpart/child/" ++ idToString id
    , expect = Http.expectJson SelectSubpartChild (decodeIdMap decodeSubpart)
    }

selectSubpartParent : Id Unit -> Cmd Msg
selectSubpartParent id =
  Http.get
    { url = baseUrl ++ "subpart/parent/" ++ idToString id
    , expect = Http.expectJson SelectSubpartParent (decodeIdMap decodeSubpart)
    }

insertSubpart : Subpart -> Cmd Msg
insertSubpart subpart =
  Http.post
    { url = baseUrl ++ "subpart"
    , body = Http.jsonBody (encodeSubpart subpart)
    , expect = Http.expectJson InsertSubpart decodeId
    }

deleteSubpart : Id Subpart -> Cmd Msg
deleteSubpart = delete "subpart" DeleteSubpart

--------------------------------------------------------------------------------
-- Message

sendMessageMember : NewMessage Member -> Cmd Msg
sendMessageMember msg =
  Http.post
    { url = baseUrl ++ "message/member/send"
    , body = Http.jsonBody <| encodeNewMessage encodeMember msg
    , expect = Http.expectJson SendMessageMember decodeId
    }

sendReplyMember : NewReply Member -> Cmd (Response ())
sendReplyMember reply =
  Http.post
    { url = baseUrl ++ "message/member/reply"
    , body = Http.jsonBody <| encodeNewReply reply
    , expect = Http.expectJson ignoreResult decodeId
    }

viewReplyMember : Id (Reply Member) -> Cmd (Response ())
viewReplyMember id =
  Http.post
    { url = baseUrl ++ "message/member/view"
    , body = Http.jsonBody <| encodeId id
    , expect = Http.expectJson identity decodeNull
    }

sendMessageSubpart : NewMessage Subpart -> Cmd Msg
sendMessageSubpart msg =
  Http.post
    { url = baseUrl ++ "message/subpart/send"
    , body = Http.jsonBody <| encodeNewMessage encodeSubpart msg
    , expect = Http.expectJson SendMessageSubpart decodeId
    }

sendReplySubpart : NewReply Subpart -> Cmd (Response ())
sendReplySubpart reply =
  Http.post
    { url = baseUrl ++ "message/subpart/reply"
    , body = Http.jsonBody <| encodeNewReply reply
    , expect = Http.expectJson ignoreResult decodeId
    }

viewReplySubpart : Id (Reply Subpart) -> Cmd (Response ())
viewReplySubpart id =
  Http.post
    { url = baseUrl ++ "message/subpart/view"
    , body = Http.jsonBody <| encodeId id
    , expect = Http.expectJson identity decodeNull
    }

personInbox : Id Person -> Cmd Msg
personInbox id =
  Http.get
    { url = baseUrl ++ "message/inbox/person/" ++ idToString id
    , expect = Http.expectJson PersonInbox decodeInbox
    }

unitInbox : Id Unit -> Cmd Msg
unitInbox id =
  Http.get
    { url = baseUrl ++ "message/inbox/unit/" ++ idToString id
    , expect = Http.expectJson (UnitInbox id) decodeInbox
    }

--------------------------------------------------------------------------------
-- Delete

delete : String -> (Result Http.Error () -> Msg) -> Id a -> Cmd Msg
delete name msg id =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = baseUrl ++ name ++ "/" ++ idToString id
    , body = Http.emptyBody
    , expect = Http.expectJson msg decodeNull
    , timeout = Nothing
    , tracker = Nothing
    }
