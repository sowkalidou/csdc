module CSDC.Page exposing
  ( Page (..)
  , Info
  , goTo
  , fromFragment
  )

import CSDC.Types exposing (..)
import CSDC.API as API

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Maybe
import Maybe exposing (withDefault)
import String
import Url exposing (Url)

--------------------------------------------------------------------------------
-- Model

type Page
  = Explorer
  | Studio
  | Person (Id Person)
  | Unit (Id Unit)
  | Admin
  | UnitAdmin (Id Unit)
  | InvitationMember (Id Person)
  | MessageSubpart (Id Person) (Id Unit) MessageType

type alias Info =
  { key : Nav.Key
  , url : Url
  }

goTo : Info -> Page -> Cmd msg
goTo { key, url } page =
  let
    newUrl =
      { url
      | fragment = Just (toFragment page)
      }
  in
    Nav.pushUrl key (Url.toString newUrl)

--------------------------------------------------------------------------------
-- Conversion to URL fragment

toFragment : Page -> String
toFragment page = String.join "/" ("" :: toFragments page)

toFragments : Page -> List String
toFragments page =
  case page of
    Explorer -> ["explorer"]
    Studio -> ["studio"]
    Person uid -> ["person", toFragmentId uid]
    Unit uid -> ["unit", toFragmentId uid]
    Admin -> ["admin"]
    UnitAdmin uid -> ["unit-admin", toFragmentId uid]
    InvitationMember pid ->
      [ "invitation-member"
      , toFragmentId pid
      ]
    MessageSubpart mid uid mtype ->
      [ "message-subpart"
      , toFragmentId mid
      , toFragmentId uid
      , toFragmentMessageType mtype
      ]

toFragmentId : Id a -> String
toFragmentId (Id a) = String.fromInt a

toFragmentMessageType : MessageType -> String
toFragmentMessageType mtype =
  case mtype of
    Invitation -> "invitation"
    Submission -> "submission"

--------------------------------------------------------------------------------
-- Conversion from URL fragment

fromFragment : String -> Page
fromFragment fragment =
  fromFragments <| List.drop 1 (String.split "/" fragment)

fromFragments : List String -> Page
fromFragments l =
  let default = Maybe.withDefault Studio in
  case l of
    [] -> Studio
    ["studio"] -> Studio
    ["explorer"] -> Explorer
    ["person", uid] ->
      default <|
      Maybe.map Person
        (fromFragmentId uid)
    ["unit", uid] ->
      default <|
      Maybe.map Unit
        (fromFragmentId uid)
    ["admin"] -> Admin
    ["unit-admin",uid] ->
      default <|
      Maybe.map UnitAdmin
        (fromFragmentId uid)
    ["invitation-member",pid] ->
      default <|
      Maybe.map InvitationMember
        (fromFragmentId pid)
    ["message-subpart",mid,uid,mtype] ->
      default <|
      Maybe.map3 MessageSubpart
        (fromFragmentId mid)
        (fromFragmentId uid)
        (fromFragmentMessageType mtype)
    _ -> Studio

fromFragmentId : String -> Maybe (Id a)
fromFragmentId s = Maybe.map Id (String.toInt s)

fromFragmentMessageType : String -> Maybe MessageType
fromFragmentMessageType s =
  case s of
    "invitation" -> Just Invitation
    "submission" -> Just Submission
    _ -> Nothing
