module CSDC.Types exposing (..)

import Dict
import Dict exposing (Dict)
import Json.Decode as Decoder
import Json.Decode exposing (Decoder)
import Json.Encode as Encoder
import Json.Encode exposing (Value)
import String
import Tuple exposing (pair)

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
idFromString s =
  case String.toInt s of
    Nothing -> Nothing
    Just n -> Just (Id n)

type alias WithId a =
  { id : Id a
  , value : a
  }

decodeWithId : Decoder a -> Decoder (WithId a)
decodeWithId decode =
  Decoder.map2 WithId
    (Decoder.field "id" decodeId)
    (Decoder.field "value" decode)

--------------------------------------------------------------------------------
-- IdMap

type IdMap a b = IdMap (Dict Int b)

encodeIdMap : (b -> Value) -> IdMap a b -> Value
encodeIdMap encode (IdMap dict) =
  let
    encodePair (id, value) =
      Encoder.list identity [ Encoder.int id, encode value ]
  in
    Encoder.list encodePair <| Dict.toList dict

decodeIdMap : Decoder b -> Decoder (IdMap a b)
decodeIdMap decode =
  let
    decodePair =
      Decoder.map2 pair (Decoder.index 0 Decoder.int) (Decoder.index 1 decode)
  in
    Decoder.map (IdMap << Dict.fromList) (Decoder.list decodePair)

idMapToList : IdMap a b -> List (Id a, b)
idMapToList (IdMap m) = List.map (\(i,a) -> (Id i, a)) (Dict.toList m)

idMapEmpty : IdMap a b
idMapEmpty = IdMap (Dict.empty)

idMapLookup : Id a -> IdMap a b -> Maybe b
idMapLookup (Id a) (IdMap b) = Dict.get a b

idMapAny : (b -> Bool) -> IdMap a b -> Bool
idMapAny pred (IdMap b) =
  List.any pred (Dict.values b)

idMapFind : (b -> Bool) -> IdMap a b -> Maybe b
idMapFind pred (IdMap b) =
  List.head <| List.filter pred (Dict.values b)

--------------------------------------------------------------------------------
-- User

type alias UserId = Id Person

--------------------------------------------------------------------------------
-- Person

type alias Person =
  { name : String
  , orcid : String
  , description : String
  }

encodePerson : Person -> Value
encodePerson person =
  Encoder.object
    [ ("name", Encoder.string person.name)
    , ("orcid", Encoder.string person.orcid)
    , ("description", Encoder.string person.description)
    ]

decodePerson : Decoder Person
decodePerson =
  Decoder.map3 Person
    (Decoder.field "name" Decoder.string)
    (Decoder.field "orcid" Decoder.string)
    (Decoder.field "description" Decoder.string)

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

type alias PersonInfo =
  { id : Id Person
  , person : Person
  , members : IdMap Member (WithId Unit)
  }

decodePersonInfo : Decoder PersonInfo
decodePersonInfo =
  Decoder.map3 PersonInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "person" decodePerson)
    (Decoder.field "members" (decodeIdMap (decodeWithId decodeUnit)))

-- List units of which the person is chair.
personInfoChair : PersonInfo -> List (WithId Unit)
personInfoChair info =
  let
    f (_, unit) =
      if unit.value.chair == info.id
      then Just { id = unit.id, value = unit.value }
      else Nothing
  in
    List.filterMap f <| idMapToList info.members

--------------------------------------------------------------------------------
-- Unit

type alias Unit =
  { name : String
  , description : String
  , chair : Id Person
  }

encodeUnit : Unit -> Value
encodeUnit unit =
  Encoder.object
    [ ("name", Encoder.string unit.name)
    , ("description", Encoder.string unit.description)
    , ("chair", encodeId unit.chair)
    ]

decodeUnit : Decoder Unit
decodeUnit =
  Decoder.map3 Unit
    (Decoder.field "name" Decoder.string)
    (Decoder.field "description" Decoder.string)
    (Decoder.field "chair" decodeId)

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

type alias UnitInfo =
  { id : Id Unit
  , unit : Unit
  , members : IdMap Member (WithId Person)
  , children : IdMap Subpart (WithId Unit)
  , parents : IdMap Subpart (WithId Unit)
  }

decodeUnitInfo : Decoder UnitInfo
decodeUnitInfo =
  Decoder.map5 UnitInfo
    (Decoder.field "id" decodeId)
    (Decoder.field "unit" decodeUnit)
    (Decoder.field "members" (decodeIdMap (decodeWithId decodePerson)))
    (Decoder.field "children" (decodeIdMap (decodeWithId decodeUnit)))
    (Decoder.field "parents" (decodeIdMap (decodeWithId decodeUnit)))

--------------------------------------------------------------------------------
-- Member

type alias Member =
  { person : Id Person
  , unit: Id Unit
  }

encodeMember : Member -> Value
encodeMember member =
  Encoder.object
    [ ("person", encodeId member.person)
    , ("unit", encodeId member.unit)
    ]

decodeMember : Decoder Member
decodeMember =
  Decoder.map2 Member
    (Decoder.field "person" decodeId)
    (Decoder.field "unit" decodeId)

--------------------------------------------------------------------------------
-- Subpart

type alias Subpart =
  { child: Id Unit
  , parent: Id Unit
  }

encodeSubpart : Subpart -> Value
encodeSubpart subpart =
  Encoder.object
    [ ("child", encodeId subpart.child)
    , ("parent", encodeId subpart.parent)
    ]

decodeSubpart : Decoder Subpart
decodeSubpart =
  Decoder.map2 Subpart
    (Decoder.field "child" decodeId)
    (Decoder.field "parent" decodeId)

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

encodeMessage : (a -> Value) -> Message a -> Value
encodeMessage encode m =
  Encoder.object
    [ ("type", encodeMessageType m.mtype)
    , ("text", Encoder.string m.text)
    , ("status", encodeMessageStatus m.status)
    , ("value", encode m.value)
    ]

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
  { mtype : MessageType
  , text : String
  , status : MessageStatus
  , value : a
  , left : String
  , right : String
  }

encodeMessageInfo : (a -> Value) -> MessageInfo a -> Value
encodeMessageInfo encode m =
  Encoder.object
    [ ("type", encodeMessageType m.mtype)
    , ("text", Encoder.string m.text)
    , ("status", encodeMessageStatus m.status)
    , ("value", encode m.value)
    , ("left", Encoder.string m.left)
    , ("right", Encoder.string m.right)
    ]

decodeMessageInfo : Decoder a -> Decoder (MessageInfo a)
decodeMessageInfo decode =
  Decoder.map6 MessageInfo
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

encodeReply : Reply a -> Value
encodeReply m =
  Encoder.object
    [ ("type", encodeReplyType m.rtype)
    , ("mtype", encodeMessageType m.mtype)
    , ("text", Encoder.string m.text)
    , ("status", encodeReplyStatus m.status)
    , ("id", encodeId m.id)
    ]

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
  { rtype : ReplyType
  , mtype : MessageType
  , text : String
  , status : ReplyStatus
  , message : MessageInfo a
  }

encodeReplyInfo : (a -> Value) -> ReplyInfo a -> Value
encodeReplyInfo encode m =
  Encoder.object
    [ ("type", encodeReplyType m.rtype)
    , ("mtype", encodeMessageType m.mtype)
    , ("text", Encoder.string m.text)
    , ("status", encodeReplyStatus m.status)
    , ("message", encodeMessageInfo encode m.message)
    ]

decodeReplyInfo : Decoder a -> Decoder (ReplyInfo a)
decodeReplyInfo decode =
  Decoder.map5 ReplyInfo
    (Decoder.field "type" decodeReplyType)
    (Decoder.field "mtype" decodeMessageType)
    (Decoder.field "text" Decoder.string)
    (Decoder.field "status" decodeReplyStatus)
    (Decoder.field "message" (decodeMessageInfo decode))

--------------------------------------------------------------------------------
-- Inbox

type alias Inbox =
  { messageMember : IdMap (Message Member) (MessageInfo Member)
  , replyMember : IdMap (Reply Member) (ReplyInfo Member)
  , messageSubpart : IdMap (Message Subpart) (MessageInfo Subpart)
  , replySubpart : IdMap (Reply Subpart) (ReplyInfo Subpart)
  }

emptyInbox : Inbox
emptyInbox =
  { messageMember = idMapEmpty
  , replyMember = idMapEmpty
  , messageSubpart = idMapEmpty
  , replySubpart = idMapEmpty
  }

encodeInbox : Inbox -> Value
encodeInbox inbox =
  Encoder.object
    [ ("messageMember", encodeIdMap (encodeMessageInfo encodeMember) inbox.messageMember)
    , ("replyMember", encodeIdMap (encodeReplyInfo encodeMember) inbox.replyMember)
    , ("messageSubpart", encodeIdMap (encodeMessageInfo encodeSubpart) inbox.messageSubpart)
    , ("replySubpart", encodeIdMap (encodeReplyInfo encodeSubpart) inbox.replySubpart)
    ]

decodeInbox : Decoder Inbox
decodeInbox =
  Decoder.map4 Inbox
    (Decoder.field "messageMember" (decodeIdMap <| decodeMessageInfo decodeMember))
    (Decoder.field "replyMember" (decodeIdMap <| decodeReplyInfo decodeMember))
    (Decoder.field "messageSubpart" (decodeIdMap <| decodeMessageInfo decodeSubpart))
    (Decoder.field "replySubpart" (decodeIdMap <| decodeReplyInfo decodeSubpart))

--------------------------------------------------------------------------------
-- Helpers

decodeString : (String -> Decoder a) -> Decoder a
decodeString f = Decoder.andThen f Decoder.string
