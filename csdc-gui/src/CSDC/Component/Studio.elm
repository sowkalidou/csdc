module CSDC.Component.Studio exposing
  ( Model
  , initial
  , setup
  , Msg
  , update
  , view
  )

import CSDC.API as API
import CSDC.Input
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import String

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { person : Maybe Person
  , notification : Notification
  }

initial : Model
initial =
  { person = Nothing
  , notification = Notification.Empty
  }

setup : Id Person -> Cmd Msg
setup id = Cmd.map APIMsg <| API.selectPerson id

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    APIMsg apimsg ->
      case apimsg of
        API.SelectPerson result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok person ->
              ( { model | person = Just person }
              , Cmd.none
              )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

view : Model -> List (Element Msg)
view model =
  case model.person of
    Nothing ->
      [ text "Loading..."
      ] ++
      Notification.view model.notification

    Just person ->
      [ row
          [ Font.bold, Font.size 30 ]
          [ text "Studio" ]
      , row []
          [ text person.name ]
      , row []
          [ el [ Font.bold ] (text "ORCID ID: ")
          , newTabLink []
              { url = "https://orcid.org/" ++ person.orcid
              , label = text person.orcid
              }
          ]
      ] ++
      Notification.view model.notification
