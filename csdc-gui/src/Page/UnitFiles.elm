module Page.UnitFiles exposing
  ( Model
  , setup
  , initial
  , Msg (..)
  , update
  , view
  )

import API as API
import UI.BoxFile as BoxFile
import UI.Modal as Modal
import UI.Column as Column
import UI.Preview as Preview
import Notification exposing (Notification)
import Page as Page exposing (UnitTab (..))
import Types exposing (..)
import Form

import Browser.Dom as Dom
import Delay
import File exposing (File)
import File.Select
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { files : List FileUI
  , notification : Notification
  }

initial : Model
initial =
  { files = []
  , notification = Notification.Empty
  }

setup : Id Unit -> Cmd Msg
setup id = Cmd.batch
  [ Cmd.map SetFiles <| API.getUnitFiles id
  ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = SetFiles (API.Response (List FileUI))
  | UploadedFile (API.Response ())
  | FileRequested
  | FileSelected File
  | ResetNotification

update : UnitInfo -> Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update info pageInfo msg model =
  let
    onSuccess = Notification.withResponse ResetNotification model
  in
  case msg of
    SetFiles result -> onSuccess result <| \files ->
      ( { model | files = files }
      , Cmd.none
      )

    UploadedFile result -> onSuccess result <| \_ ->
      ( { model | notification = Notification.Success }
      , Cmd.batch [ setup info.id, Notification.reset ResetNotification ]
      )

    FileRequested ->
      ( model
      , File.Select.file [] FileSelected
      )

    FileSelected file ->
      if File.size file > 1048576
      then
        ( { model | notification = Notification.Error ["File cannot be larger than 1MiB"] }
        , Notification.reset ResetNotification
        )
      else
        ( { model | notification = Notification.Processing }
        , Cmd.map UploadedFile <| API.insertUnitFile info.id file
        )

    ResetNotification ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : UnitInfo -> Model -> List (Html Msg)
view unit model =
  [ Html.div
      [ Html.Attributes.class "columns"
      , Html.Attributes.style "height" "100%"
      ]
      [ Html.div
          [ Html.Attributes.class "column is-half" ]
          [ Column.view "Files"
            ( if unit.isAdmin
              then [smallButton]
              else []
            ) <|
            viewFiles model.files
          ]
      ]
  ] ++
  Notification.view model.notification

viewFiles : List FileUI -> List (Html Msg)
viewFiles = List.map BoxFile.view

smallButton : Html Msg
smallButton =
  Html.button
    [ Html.Attributes.class "button is-success is-small"
    , Html.Events.onClick FileRequested
    ]
    [ Html.text "Upload File"
    ]
