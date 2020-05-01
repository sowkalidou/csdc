module CSDC.Component.Studio exposing
  ( Model
  , initial
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
  { id : Maybe UserId
  , person : Maybe (User Person)
  , notification : Notification
  }

initial : () -> (Model, Cmd Msg)
initial _ =
  ( { id = Nothing
    , person = Nothing
    , notification = Notification.Empty
    }
  , Cmd.map APIMsg API.rootPerson
  )

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    APIMsg apimsg ->
      case apimsg of
        API.RootPerson result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok id ->
              case id of
                Admin ->
                  ( { model | id = Just Admin, person = Just Admin }
                  , Cmd.none
                  )
                User pid ->
                  ( { model | id = Just (User pid) }
                  , Cmd.map APIMsg <| API.selectPerson pid
                  )

        API.SelectPerson result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok person ->
              ( { model | person = Just (User person) }
              , Cmd.none
              )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

view : Model -> List (Element Msg)
view model =
  [ row
      [ Font.bold, Font.size 30 ]
      [ text "Studio" ]
  ] ++
  ( case model.person of
      Nothing ->
        [ text "Loading..." ]
      Just Admin ->
        [ text "You are the Admin." ]
      Just (User person) ->
        [ row []
            [ text person.name ]
        , row []
            [ text person.orcid ]
        ]
  ) ++
  Notification.view model.notification
