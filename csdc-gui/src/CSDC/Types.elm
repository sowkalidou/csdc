module CSDC.Types exposing (..)

import Json.Decode as Decoder exposing (Decoder)
import Json.Encode as Encoder exposing (Value)
import String

--------------------------------------------------------------------------------
-- Id

type Id a = Id Int

encodeId : Id a -> Value
encodeId (Id n) = Encoder.int n

decodeId : Decoder (Id a)
decodeId = Decoder.map Id Decoder.int

idToString : Id a -> String
idToString (Id n) = String.fromInt n

idFromString : String -> Maybe (Id a)
idFromString s = Maybe.map Id (String.toInt s)

type alias WithId a =
  { id : Id a
  , value : a
  }

decodeWithId : Decoder a -> Decoder (WithId a)
decodeWithId decode =
  Decoder.map2 WithId
    (Decoder.field "id" decodeId)
    (Decoder.field "value" decode)

lookup : (a -> Bool) -> List a -> Maybe a
lookup p = List.head << List.filter p

lookupById : Id a -> List { obj | id : Id a } -> Maybe { obj | id : Id a }
lookupById id = lookup (\obj -> obj.id == id)

--------------------------------------------------------------------------------
-- User

type alias UserId = Id Person

--------------------------------------------------------------------------------
-- Person

type alias Person =
  { name : String
  , orcid : String
  , description : String
  , image : String
  , createdAt : String
  }

decodePerson : Decoder Person
decodePerson =
  Decoder.map5 Person
    (Decoder.field "name" Decoder.string)
    (Decoder.field "orcid" Decoder.string)
    (Decoder.field "description" Decoder.string)
    (Decoder.field "image" Decoder.string)
    (Decoder.field "createdAt" Decoder.string)

type alias PersonUpdate =
  { name : String
  , description : String
  }

encodePersonUpdate : PersonUpdate -> Value
encodePersonUpdate person =
  Encoder.object
    [ ("name", Encoder.string person.name)
    , ("description", Encoder.string person.description)
    ]

type alias PersonMember =
  { member : Id Member
  , id : Id Unit
  , unit : Unit
  }

decodePersonMember : Decoder PersonMember
decodePersonMember =
  Decoder.map3 PersonMember
    (Decoder.field "member" decodeId)
    (Decoder.field "id" decodeId)
    (Decoder.field "unit" decodeUnit)

type alias PersonInfo =
  { id : Id Person
  , person : Person
  , members : List PersonMember
  , unitsForMessage : List (WithId Unit)
  }

decodePersonInfo : Decoder PersonInfo
decodePersonInfo =
  Decoder.map4 PersonInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "person" decodePerson)
    (Decoder.field "members" (Decoder.list decodePersonMember))
    (Decoder.field "unitsForMessage" (Decoder.list (decodeWithId decodeUnit)))

--------------------------------------------------------------------------------
-- Unit

type alias Unit =
  { name : String
  , description : String
  , chair : Id Person
  , image : String
  , createdAt : String
  }

decodeUnit : Decoder Unit
decodeUnit =
  Decoder.map5 Unit
    (Decoder.field "name" Decoder.string)
    (Decoder.field "description" Decoder.string)
    (Decoder.field "chair" decodeId)
    (Decoder.field "image" Decoder.string)
    (Decoder.field "createdAt" Decoder.string)

type alias NewUnit =
  { name : String
  , description : String
  }

encodeNewUnit : NewUnit -> Value
encodeNewUnit unit =
  Encoder.object
    [ ("name", Encoder.string unit.name)
    , ("description", Encoder.string unit.description)
    ]

type alias UnitUpdate =
  { name : String
  , description : String
  }

encodeUnitUpdate : UnitUpdate -> Value
encodeUnitUpdate unit =
  Encoder.object
    [ ("name", Encoder.string unit.name)
    , ("description", Encoder.string unit.description)
    ]

type alias UnitMember =
  { member : Id Member
  , id : Id Person
  , person : Person
  }

decodeUnitMember : Decoder UnitMember
decodeUnitMember =
  Decoder.map3 UnitMember
    (Decoder.field "member" decodeId)
    (Decoder.field "id" decodeId)
    (Decoder.field "person" decodePerson)

type alias UnitSubpart =
  { subpart : Id Subpart
  , id : Id Unit
  , unit : Unit
  }

decodeUnitSubpart : Decoder UnitSubpart
decodeUnitSubpart =
  Decoder.map3 UnitSubpart
    (Decoder.field "subpart" decodeId)
    (Decoder.field "id" decodeId)
    (Decoder.field "unit" decodeUnit)

type alias UnitInfo =
  { id : Id Unit
  , unit : Unit
  , members : List UnitMember
  , children : List UnitSubpart
  , parents : List UnitSubpart
  , user : Id Person
  , isMember : Bool
  , isAdmin : Bool
  , isMembershipPending : Bool
  , unitsForMessage : List (WithId Unit)
  }

decodeUnitInfo : Decoder UnitInfo
decodeUnitInfo =
  Decoder.succeed UnitInfo
    |> andMap (Decoder.field "id" decodeId)
    |> andMap (Decoder.field "unit" decodeUnit)
    |> andMap (Decoder.field "members" (Decoder.list decodeUnitMember ))
    |> andMap (Decoder.field "children" (Decoder.list decodeUnitSubpart))
    |> andMap (Decoder.field "parents" (Decoder.list decodeUnitSubpart))
    |> andMap (Decoder.field "user" decodeId)
    |> andMap (Decoder.field "isMember" Decoder.bool)
    |> andMap (Decoder.field "isAdmin" Decoder.bool)
    |> andMap (Decoder.field "isMembershipPending" Decoder.bool)
    |> andMap (Decoder.field "unitsForMessage" (Decoder.list (decodeWithId decodeUnit)))

andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap = Decoder.map2 (\a f -> f a)

--------------------------------------------------------------------------------
-- Member

type alias Member =
  { person : Id Person
  , unit: Id Unit
  , createdAt: String
  }

decodeMember : Decoder Member
decodeMember =
  Decoder.map3 Member
    (Decoder.field "person" decodeId)
    (Decoder.field "unit" decodeId)
    (Decoder.field "createdAt" Decoder.string)

type alias NewMember =
  { person : Id Person
  , unit: Id Unit
  }

encodeNewMember : NewMember -> Value
encodeNewMember member =
  Encoder.object
    [ ("person", encodeId member.person)
    , ("unit", encodeId member.unit)
    ]

decodeNewMember : Decoder NewMember
decodeNewMember =
  Decoder.map2 NewMember
    (Decoder.field "person" decodeId)
    (Decoder.field "unit" decodeId)

--------------------------------------------------------------------------------
-- Subpart

type alias Subpart =
  { child: Id Unit
  , parent: Id Unit
  , createdAt: String
  }

decodeSubpart : Decoder Subpart
decodeSubpart =
  Decoder.map3 Subpart
    (Decoder.field "child" decodeId)
    (Decoder.field "parent" decodeId)
    (Decoder.field "createdAt" Decoder.string)

type alias NewSubpart =
  { child: Id Unit
  , parent: Id Unit
  }

decodeNewSubpart : Decoder NewSubpart
decodeNewSubpart =
  Decoder.map2 NewSubpart
    (Decoder.field "child" decodeId)
    (Decoder.field "parent" decodeId)

encodeNewSubpart : NewSubpart -> Value
encodeNewSubpart subpart =
  Encoder.object
    [ ("child", encodeId subpart.child)
    , ("parent", encodeId subpart.parent)
    ]

--------------------------------------------------------------------------------
-- Message

type MessageStatus = Waiting | Accepted | Rejected

decodeMessageStatus : Decoder MessageStatus
decodeMessageStatus =
  decodeString <| \s ->
    case s of
      "Waiting" ->
        Decoder.succeed Waiting
      "Accepted" ->
        Decoder.succeed Accepted
      "Rejected" ->
        Decoder.succeed Rejected
      _ ->
        Decoder.fail <| "Invalid MessageStatus: " ++ s

encodeMessageStatus : MessageStatus -> Value
encodeMessageStatus s =
  case s of
    Waiting -> Encoder.string "Waiting"
    Accepted -> Encoder.string "Accepted"
    Rejected -> Encoder.string "Rejected"

type MessageType = Invitation | Submission

decodeMessageType : Decoder MessageType
decodeMessageType =
  decodeString <| \s ->
    case s of
      "Invitation" ->
        Decoder.succeed Invitation
      "Submission" ->
        Decoder.succeed Submission
      _ ->
        Decoder.fail <| "Invalid MessageType: " ++ s

encodeMessageType : MessageType -> Value
encodeMessageType s =
  case s of
    Invitation -> Encoder.string "Invitation"
    Submission -> Encoder.string "Submission"

type alias Message a =
  { mtype : MessageType
  , text : String
  , status : MessageStatus
  , value : a
  }

decodeMessage : Decoder a -> Decoder (Message a)
decodeMessage decode =
  Decoder.map4 Message
    (Decoder.field "type" decodeMessageType)
    (Decoder.field "text" Decoder.string)
    (Decoder.field "status" decodeMessageStatus)
    (Decoder.field "value" decode)

type alias NewMessage a =
  { mtype : MessageType
  , text : String
  , value : a
  }

encodeNewMessage : (a -> Value) -> NewMessage a -> Value
encodeNewMessage encode m =
  Encoder.object
    [ ("type", encodeMessageType m.mtype)
    , ("text", Encoder.string m.text)
    , ("value", encode m.value)
    ]

type alias MessageInfo a =
  { id : Id (Message a)
  , mtype : MessageType
  , text : String
  , status : MessageStatus
  , value : a
  , left : String
  , right : String
  }

decodeMessageInfo : Decoder a -> Decoder (MessageInfo a)
decodeMessageInfo decode =
  Decoder.map7 MessageInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "type" decodeMessageType)
    (Decoder.field "text" Decoder.string)
    (Decoder.field "status" decodeMessageStatus)
    (Decoder.field "value" decode)
    (Decoder.field "left" Decoder.string)
    (Decoder.field "right" Decoder.string)

--------------------------------------------------------------------------------
-- Reply

type ReplyStatus = Seen | NotSeen

decodeReplyStatus : Decoder ReplyStatus
decodeReplyStatus =
  decodeString <| \s ->
    case s of
      "Seen" ->
        Decoder.succeed Seen
      "NotSeen" ->
        Decoder.succeed NotSeen
      _ ->
        Decoder.fail <| "Invalid ReplyStatus: " ++ s

encodeReplyStatus : ReplyStatus -> Value
encodeReplyStatus s =
  case s of
    Seen -> Encoder.string "Seen"
    NotSeen -> Encoder.string "NotSeen"

type ReplyType = Accept | Reject

decodeReplyType : Decoder ReplyType
decodeReplyType =
  decodeString <| \s ->
    case s of
      "Accept" ->
        Decoder.succeed Accept
      "Reject" ->
        Decoder.succeed Reject
      _ ->
        Decoder.fail <| "Invalid ReplyType: " ++ s

encodeReplyType : ReplyType -> Value
encodeReplyType s =
  case s of
    Accept -> Encoder.string "Accept"
    Reject -> Encoder.string "Reject"

type alias Reply a =
  { rtype : ReplyType
  , mtype : MessageType
  , text : String
  , status : ReplyStatus
  , id : Id (Message a)
  }

decodeReply : Decoder (Reply a)
decodeReply =
  Decoder.map5 Reply
    (Decoder.field "type" decodeReplyType)
    (Decoder.field "mtype" decodeMessageType)
    (Decoder.field "text" Decoder.string)
    (Decoder.field "status" decodeReplyStatus)
    (Decoder.field "id" decodeId)

type alias NewReply a =
  { rtype : ReplyType
  , text : String
  , message : Id (Message a)
  }

encodeNewReply : NewReply a -> Value
encodeNewReply m =
  Encoder.object
    [ ("type", encodeReplyType m.rtype)
    , ("text", Encoder.string m.text)
    , ("message", encodeId m.message)
    ]

type alias ReplyInfo a =
  { id : Id (Reply a)
  , rtype : ReplyType
  , mtype : MessageType
  , text : String
  , status : ReplyStatus
  , message : MessageInfo a
  }

decodeReplyInfo : Decoder a -> Decoder (ReplyInfo a)
decodeReplyInfo decode =
  Decoder.map6 ReplyInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "type" decodeReplyType)
    (Decoder.field "mtype" decodeMessageType)
    (Decoder.field "text" Decoder.string)
    (Decoder.field "status" decodeReplyStatus)
    (Decoder.field "message" (decodeMessageInfo decode))

--------------------------------------------------------------------------------
-- Inbox

type alias Inbox =
  { messageMember : List (MessageInfo NewMember)
  , replyMember : List (ReplyInfo NewMember)
  , messageSubpart : List (MessageInfo NewSubpart)
  , replySubpart : List (ReplyInfo NewSubpart)
  }

emptyInbox : Inbox
emptyInbox =
  { messageMember = []
  , replyMember = []
  , messageSubpart = []
  , replySubpart = []
  }

decodeInbox : Decoder Inbox
decodeInbox =
  Decoder.map4 Inbox
    (Decoder.field "messageMember" (Decoder.list <| decodeMessageInfo decodeNewMember))
    (Decoder.field "replyMember" (Decoder.list <| decodeReplyInfo decodeNewMember))
    (Decoder.field "messageSubpart" (Decoder.list <| decodeMessageInfo decodeNewSubpart))
    (Decoder.field "replySubpart" (Decoder.list <| decodeReplyInfo decodeNewSubpart))

--------------------------------------------------------------------------------
-- Search

type SearchId = SearchUnit (Id Unit) | SearchPerson (Id Person)

decodeSearchId : Decoder SearchId
decodeSearchId =
  Decoder.field "tag" Decoder.string
    |> Decoder.andThen (\tag ->
      if tag == "SearchUnit"
      then Decoder.map SearchUnit <| Decoder.field "contents" decodeId
      else if tag == "SearchPerson"
      then Decoder.map SearchPerson <| Decoder.field "contents" decodeId
      else Decoder.fail <| "SearchId: Unknown tag " ++ tag
    )

type alias SearchResult a =
  { name : String
  , id : a
  }

decodeSearchResult : Decoder a -> Decoder (SearchResult a)
decodeSearchResult decode =
  Decoder.map2 SearchResult
    (Decoder.field "name" Decoder.string)
    (Decoder.field "id" decode)

--------------------------------------------------------------------------------
-- File

type alias Base64File =
  { name : String
  , contents : String
  }

encodeBase64File : Base64File -> Value
encodeBase64File file =
  Encoder.object
    [ ("name", Encoder.string file.name)
    , ("contents", Encoder.string file.contents)
    ]

--------------------------------------------------------------------------------
-- Forum

type alias NewThread =
  { subject : String
  , text : String
  }

encodeNewThread : NewThread -> Value
encodeNewThread newThread =
  Encoder.object
    [ ("subject", Encoder.string newThread.subject)
    , ("text", Encoder.string newThread.text)
    ]

type alias Thread =
  { unit : Id Unit
  , author : Id Person
  , subject : String
  }

type alias ThreadInfo =
  { id : Id Thread
  , unit : Id Unit
  , author : Id Person
  , authorName : String
  , subject : String
  , createdAt : String
  , last : String
  , messages : Int
  }

decodeThreadInfo : Decoder ThreadInfo
decodeThreadInfo =
  Decoder.map8 ThreadInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "unit" decodeId)
    (Decoder.field "author" decodeId)
    (Decoder.field "authorName" Decoder.string)
    (Decoder.field "subject" Decoder.string)
    (Decoder.field "createdAt" Decoder.string)
    (Decoder.field "last" Decoder.string)
    (Decoder.field "messages" Decoder.int)

type alias NewPost =
  { text : String
  }

encodeNewPost : NewPost -> Value
encodeNewPost newPost =
  Encoder.object
    [ ("text", Encoder.string newPost.text)
    ]

type alias Post =
  { thread : Id Thread
  , author : Id Person
  , text : String
  }

type alias PostInfo =
  { id : Id Post
  , author : Id Person
  , authorName : String
  , text : String
  , createdAt : String
  }

decodePostInfo : Decoder PostInfo
decodePostInfo =
  Decoder.map5 PostInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "author" decodeId)
    (Decoder.field "authorName" Decoder.string)
    (Decoder.field "text" Decoder.string)
    (Decoder.field "createdAt" Decoder.string)

--------------------------------------------------------------------------------
-- Helpers

decodeString : (String -> Decoder a) -> Decoder a
decodeString f = Decoder.andThen f Decoder.string
