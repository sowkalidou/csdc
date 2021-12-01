module CSDC.Page exposing
  ( Page (..)
  , Info
  , goTo
  , reload
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
  | UnitAdmin (Id Unit)

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

reload : Cmd msg
reload = Nav.reload

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
    UnitAdmin uid -> ["unit-admin", toFragmentId uid]

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
    ["unit-admin",uid] ->
      default <|
      Maybe.map UnitAdmin
        (fromFragmentId uid)
    _ -> Studio

fromFragmentId : String -> Maybe (Id a)
fromFragmentId s = Maybe.map Id (String.toInt s)

fromFragmentMessageType : String -> Maybe MessageType
fromFragmentMessageType s =
  case s of
    "invitation" -> Just Invitation
    "submission" -> Just Submission
    _ -> Nothing
