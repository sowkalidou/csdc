module CSDC.Form.Person exposing
  ( Model
  , initial
  , fromPerson
  , setup
  , Msg
  , updateWith
  , view
  )

import CSDC.API as API
import CSDC.Component.ImageUpload as ImageUpload
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Types exposing (..)
import CSDC.Input as Input
import Field exposing (Field)
import Validation
import Form

import Croppie
import Html exposing (Html)
import Html.Attributes
import Html.Events

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { name : Field String String
  , description : Field String String
  , image : Field (Maybe String) (Maybe String)
  , imageUpload : ImageUpload.Model
  , notification : Notification
  }

initial : Model
initial =
  { name = Field.requiredString "Name"
  , description = Field.requiredString "Description"
  , image = Field.optional "Profile Picture"
  , imageUpload = ImageUpload.initial
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
  Debug.log "AAAAA"
  { model
  | name = Field.reload model.name
  , description = Field.reload model.description
  }

parse : Model -> Maybe PersonUpdate
parse model =
  let
    result =
      Validation.andThen (Field.validate model.name) <| \name ->
      Validation.andThen (Field.validate model.description) <| \description ->
      Validation.andThen (Field.validate model.image) <| \image ->
      Validation.valid
        { name = name
        , description = description
        , image = image
        }
  in
    case Validation.validate result of
      Err _ -> Nothing
      Ok unit -> Just unit

setup : Maybe String -> Cmd Msg
setup mimage = Cmd.map (Form.ModelMsg << ImageUploadMsg) (ImageUpload.setup mimage)

--------------------------------------------------------------------------------
-- Update

type alias Config =
  { id : Id Person
  , finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update
  { initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse
  , request = API.updatePerson config.id
  , finish = \_ -> config.finish
  }

type ModelMsg
  = SetName String
  | SetDescription String
  | SetImage (Croppie.Result ModelMsg)
  | ImageUploadMsg ImageUpload.Msg

type alias Msg = Form.Msg ModelMsg () ()

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetName val ->
      ( { model | name = Field.set val model.name }
      , Cmd.none
      )
    SetDescription val ->
      ( { model | description = Field.set val model.description }
      , Cmd.none
      )
    SetImage result ->
      case result of
        Croppie.Base64 val ->
          ( { model | image = Field.set (Just val) model.image }
          , Cmd.none
          )
        _ ->
          (model, Cmd.none)
    ImageUploadMsg imsg ->
      let
        (imageUpload, cmd) = ImageUpload.update imsg model.imageUpload
      in
        ( { model | imageUpload = imageUpload }
        , Cmd.map ImageUploadMsg cmd
        )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  [ Input.wrapper model.image <|
    Html.map Form.ModelMsg <|
    ImageUpload.view model.imageUpload ImageUploadMsg SetImage
  , Input.text model.name SetName
  , Input.textarea model.description SetDescription
  , Input.button "Save" ()
  ]
