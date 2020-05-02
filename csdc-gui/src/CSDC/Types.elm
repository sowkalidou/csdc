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

--------------------------------------------------------------------------------
-- IdMap

type IdMap a b = IdMap (Dict Int b)

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

--------------------------------------------------------------------------------
-- User

type User a = Admin | User a
type alias UserId = User (Id Person)

decodeUser : Decoder a -> Decoder (User a)
decodeUser decode =
  let
    checkTag tag =
      case tag of
        "Admin" ->
          Decoder.succeed Admin
        "User" ->
          Decoder.field "contents" decode |> Decoder.map User
        _ ->
          Decoder.fail "Invalid tag for User JSON."
  in
    Decoder.field "tag" Decoder.string |> Decoder.andThen checkTag

--------------------------------------------------------------------------------
-- Person

type alias Person =
  { name : String
  , orcid: String
  }

encodePerson : Person -> Value
encodePerson person =
  Encoder.object
    [ ("name", Encoder.string person.name)
    , ("orcid", Encoder.string person.orcid)
    ]


decodePerson : Decoder Person
decodePerson =
  Decoder.map2 Person
    (Decoder.field "name" Decoder.string)
    (Decoder.field "orcid" Decoder.string)

--------------------------------------------------------------------------------
-- Unit

type alias Unit =
  { name : String
  }

encodeUnit : Unit -> Value
encodeUnit unit =
  Encoder.object
    [ ("name", Encoder.string unit.name)
    ]


decodeUnit : Decoder Unit
decodeUnit =
  Decoder.map Unit
    (Decoder.field "name" Decoder.string)

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

