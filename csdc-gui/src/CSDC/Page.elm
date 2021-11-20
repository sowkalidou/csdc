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
  | ViewPerson (Id Person)
  | ViewUnit (Id Unit)
  | Admin
  | ViewUnitAdmin (Id Unit)
  | MessageMember (Id Person) (Id Unit) MessageType
  | InvitationMember (Id Person)
  | ReplyMember (Id (Message Member)) MessageType
  | MessageSubpart (Id Person) (Id Unit) MessageType
  | ReplySubpart (Id (Message Subpart)) MessageType

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
    ViewPerson uid -> ["view-person", toFragmentId uid]
    ViewUnit uid -> ["view-unit", toFragmentId uid]
    Admin -> ["admin"]
    ViewUnitAdmin uid -> ["view-unit-admin", toFragmentId uid]
    MessageMember pid uid mtype ->
      [ "message-member"
      , toFragmentId pid
      , toFragmentId uid
      , toFragmentMessageType mtype
      ]
    InvitationMember pid ->
      [ "invitation-member"
      , toFragmentId pid
      ]
    ReplyMember mid mtype ->
      [ "reply-member"
      , toFragmentId mid
      , toFragmentMessageType mtype
      ]
    MessageSubpart mid uid mtype ->
      [ "message-subpart"
      , toFragmentId mid
      , toFragmentId uid
      , toFragmentMessageType mtype
      ]
    ReplySubpart mid mtype ->
      [ "reply-subpart"
      , toFragmentId mid
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
    ["view-person", uid] ->
      default <|
      Maybe.map ViewPerson
        (fromFragmentId uid)
    ["view-unit", uid] ->
      default <|
      Maybe.map ViewUnit
        (fromFragmentId uid)
    ["admin"] -> Admin
    ["view-unit-admin",uid] ->
      default <|
      Maybe.map ViewUnitAdmin
        (fromFragmentId uid)
    ["message-member",pid,uid,mtype] ->
      default <|
      Maybe.map3 MessageMember
        (fromFragmentId pid)
        (fromFragmentId uid)
        (fromFragmentMessageType mtype)
    ["invitation-member",pid] ->
      default <|
      Maybe.map InvitationMember
        (fromFragmentId pid)
    ["reply-member",mid,mtype] ->
      default <|
      Maybe.map2 ReplyMember
        (fromFragmentId mid)
        (fromFragmentMessageType mtype)
    ["message-subpart",mid,uid,mtype] ->
      default <|
      Maybe.map3 MessageSubpart
        (fromFragmentId mid)
        (fromFragmentId uid)
        (fromFragmentMessageType mtype)
    ["reply-subpart",mid,mtype] ->
      default <|
      Maybe.map2 ReplySubpart
        (fromFragmentId mid)
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
