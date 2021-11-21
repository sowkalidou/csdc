module CSDC.Form.Person exposing
  ( Model
  , initial
  , fromPerson
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Types exposing (..)
import CSDC.Input as Input
import Field exposing (Field)
import Validation

import Html exposing (Html)
import Html.Attributes
import Html.Events

type alias Model =
  { name : Field String String
  , description : Field String String
  , notification : Notification
  }

initial : Model
initial =
  { name = Field.requiredString "Name"
  , description = Field.requiredString "Description"
  , notification = Notification.Empty
  }

fromPerson : Person -> Model
fromPerson person =
  { initial
  | name = Field.set person.name initial.name
  , description = Field.set person.description initial.description
  }

reload : Model -> Model
reload model =
  { model
  | name = Field.reload model.name
  , description = Field.reload model.description
  }

type Msg
  = SetName String
  | SetDescription String
  | ResetNotification
  | Save
  | APIMsg API.Msg

update : (Person -> Cmd API.Msg) -> String -> Msg -> Model -> (Model, Cmd Msg)
update makeAPIMsg orcid msg model =
  case msg of
    SetName val ->
      ( { model | name = Field.set val model.name }
      , Cmd.none
      )
    SetDescription val ->
      ( { model | description = Field.set val model.description }
      , Cmd.none
      )
    ResetNotification ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )
    Save ->
      let
        reloaded = reload model

        result =
          Validation.andThen (Field.validate model.name) <| \name ->
          Validation.andThen (Field.validate model.description) <| \description ->
          Validation.valid
            { name = name
            , description = description
            , orcid = orcid
            }
      in
        case Validation.validate result of
          Err _ ->
            ( reloaded
            , Cmd.none
            )

          Ok unit ->
            ( reloaded
            , Cmd.map APIMsg (makeAPIMsg unit)
            )

    -- To be managed by the form caller
    APIMsg _ -> (model, Cmd.none)

view : Model -> Html Msg
view model =
  Html.div
    [] <|
    [ Input.text model.name SetName
    , Input.textarea model.description SetDescription
    , Input.button Save "Save"
    ] ++ Notification.view model.notification
