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
-- Person

getUserInfo : Cmd (Response PersonInfo)
getUserInfo =
  Http.get
    { url = baseUrl ++ "user/info"
    , expect = Http.expectJson identity decodePersonInfo
    }

getPersonInfo : Id Person -> Cmd (Response PersonInfo)
getPersonInfo id =
  Http.get
    { url = baseUrl ++ "person/" ++ idToString id ++ "/info"
    , expect = Http.expectJson identity decodePersonInfo
    }

unitsPerson : Cmd (Response (List (WithId Unit)))
unitsPerson =
  Http.get
    { url = baseUrl ++ "user/units"
    , expect = Http.expectJson identity (D.list (decodeWithId decodeUnit))
    }

updatePerson : Id Person -> PersonUpdate -> Cmd (Response ())
updatePerson id person =
  Http.post
    { url = baseUrl ++ "person/" ++ idToString id
    , body = Http.jsonBody (encodePersonUpdate person)
    , expect = Http.expectJson identity decodeNull
    }

--------------------------------------------------------------------------------
-- Unit

getUnitInfo : Id Unit -> Cmd (Response UnitInfo)
getUnitInfo id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/info"
    , expect = Http.expectJson identity decodeUnitInfo
    }

getUnitChildren : Id Unit -> Cmd (Response (List UnitSubpart))
getUnitChildren id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/children"
    , expect = Http.expectJson identity (D.list decodeUnitSubpart)
    }

getUnitParents : Id Unit -> Cmd (Response (List UnitSubpart))
getUnitParents id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id ++ "/parents"
    , expect = Http.expectJson identity (D.list decodeUnitSubpart)
    }

createUnit : NewUnit -> Cmd (Response (Id Unit))
createUnit unit =
  Http.post
    { url = baseUrl ++ "unit"
    , body = Http.jsonBody (encodeNewUnit unit)
    , expect = Http.expectJson identity decodeId
    }

selectUnit : Id Unit -> Cmd (Response Unit)
selectUnit id =
  Http.get
    { url = baseUrl ++ "unit/" ++ idToString id
    , expect = Http.expectJson identity decodeUnit
    }

updateUnit : Id Unit -> UnitUpdate -> Cmd (Response ())
updateUnit id unit =
  Http.post
    { url = baseUrl ++ "unit/" ++ idToString id
    , body = Http.jsonBody (encodeUnitUpdate unit)
    , expect = Http.expectJson identity decodeNull
    }

deleteUnit : Id Unit -> Cmd (Response ())
deleteUnit = delete "unit" identity

--------------------------------------------------------------------------------
-- Member

insertMember : NewMember -> Cmd (Response (Id Member))
insertMember member =
  Http.post
    { url = baseUrl ++ "member"
    , body = Http.jsonBody (encodeNewMember member)
    , expect = Http.expectJson identity decodeId
    }

deleteMember : Id Member -> Cmd (Response ())
deleteMember = delete "member" identity

--------------------------------------------------------------------------------
-- Subpart

insertSubpart : NewSubpart -> Cmd (Response (Id Subpart))
insertSubpart subpart =
  Http.post
    { url = baseUrl ++ "subpart"
    , body = Http.jsonBody (encodeNewSubpart subpart)
    , expect = Http.expectJson identity decodeId
    }

deleteSubpart : Id Subpart -> Cmd (Response ())
deleteSubpart = delete "subpart" identity

--------------------------------------------------------------------------------
-- Message

sendMessageMember : NewMessage NewMember -> Cmd (Response (Id (Message NewMember)))
sendMessageMember msg =
  Http.post
    { url = baseUrl ++ "message/member/send"
    , body = Http.jsonBody <| encodeNewMessage encodeNewMember msg
    , expect = Http.expectJson identity decodeId
    }

sendReplyMember : NewReply NewMember -> Cmd (Response ())
sendReplyMember reply =
  Http.post
    { url = baseUrl ++ "message/member/reply"
    , body = Http.jsonBody <| encodeNewReply reply
    , expect = Http.expectJson ignoreResult decodeId
    }

viewReplyMember : Id (Reply NewMember) -> Cmd (Response ())
viewReplyMember id =
  Http.post
    { url = baseUrl ++ "message/member/view"
    , body = Http.jsonBody <| encodeId id
    , expect = Http.expectJson identity decodeNull
    }

sendMessageSubpart : NewMessage NewSubpart -> Cmd (Response (Id (Message NewSubpart)))
sendMessageSubpart msg =
  Http.post
    { url = baseUrl ++ "message/subpart/send"
    , body = Http.jsonBody <| encodeNewMessage encodeNewSubpart msg
    , expect = Http.expectJson identity decodeId
    }

sendReplySubpart : NewReply NewSubpart -> Cmd (Response ())
sendReplySubpart reply =
  Http.post
    { url = baseUrl ++ "message/subpart/reply"
    , body = Http.jsonBody <| encodeNewReply reply
    , expect = Http.expectJson ignoreResult decodeId
    }

viewReplySubpart : Id (Reply NewSubpart) -> Cmd (Response ())
viewReplySubpart id =
  Http.post
    { url = baseUrl ++ "message/subpart/view"
    , body = Http.jsonBody <| encodeId id
    , expect = Http.expectJson identity decodeNull
    }

personInbox : Id Person -> Cmd (Response Inbox)
personInbox id =
  Http.get
    { url = baseUrl ++ "message/inbox/person/" ++ idToString id
    , expect = Http.expectJson identity decodeInbox
    }

unitInbox : Id Unit -> Cmd (Response Inbox)
unitInbox id =
  Http.get
    { url = baseUrl ++ "message/inbox/unit/" ++ idToString id
    , expect = Http.expectJson identity decodeInbox
    }

--------------------------------------------------------------------------------
-- Delete

delete : String -> (Result Http.Error () -> b) -> Id a -> Cmd b
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
