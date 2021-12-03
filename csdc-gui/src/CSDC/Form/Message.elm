module CSDC.Form.Message exposing
  ( Model
  , initial
  , fromUnitInfo
  , fromPersonInfo
  , Msg
  , updateWith
  , view
  )

import CSDC.API as API
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Types exposing (..)
import CSDC.Input as Input
import Field exposing (Field)
import Form
import Validation

import Html exposing (Html)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { reason: Field String String
  , id : Field (Maybe (Id Unit)) (Id Unit)
  , units: List (WithId Unit)
  , notification : Notification
  }

initial : Model
initial =
  { reason = Field.requiredString "Message"
  , id = Field.required "Unit"
  , units = []
  , notification = Notification.Empty
  }

fromUnitInfo : UnitInfo -> Model
fromUnitInfo info =
  { initial
  | units = info.unitsForMessage
  }

fromPersonInfo : PersonInfo -> Model
fromPersonInfo info =
  { initial
  | units = info.unitsForMessage
  }

reload : Model -> Model
reload model =
  { model
  | id = Field.reload model.id
  , reason = Field.reload model.reason
  }

parse : (MessageType, Id Unit -> a) -> Model -> Maybe (NewMessage a)
parse (mtype, make) model =
  let
    validation =
      Validation.andThen (Field.validate model.id) <| \id ->
      Validation.andThen (Field.validate model.reason) <| \reason ->
      Validation.valid
        { mtype = mtype
        , text = reason
        , value = make id
        }
  in
    case Validation.validate validation of
      Err _ -> Nothing
      Ok reason -> Just reason

--------------------------------------------------------------------------------
-- Update

type alias Config a =
  { request : NewMessage a -> Cmd (API.Response (Id (Message a)))
  , finish : Cmd (Msg a)
  }

updateWith : Config a -> Msg a -> Model -> (Model, Cmd (Msg a))
updateWith config = Form.update
  { initial = initial
  , update = update
  , reload = reload
  , parse = parse
  , request = config.request
  , finish = \_ -> config.finish
  }

type alias Msg a = Form.Msg ModelMsg (MessageType, Id Unit -> a) (Id (Message a))

type ModelMsg
  = SetReason String
  | SetId (Maybe (Id Unit))

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetReason val ->
      ( { model | reason = Field.set val model.reason }
      , Cmd.none
      )
    SetId val ->
      ( { model | id = Field.set val model.id }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

toChoices : List (WithId Unit) -> List (Id Unit, String)
toChoices = List.map <| \unit -> (unit.id, unit.value.name)

view : MessageType -> (Id Unit -> a) -> Model -> List (Html (Msg a))
view mtype make model =
  [ Html.p [] [ Html.i [] [ Html.text "Please write a message below." ] ]
  , Input.select (toChoices model.units) model.id SetId
  , Input.textarea model.reason SetReason
  , Input.button "Send" (mtype, make)
  ]
