module CSDC.Form.Image exposing
  ( Model
  , initial
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

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { image : Field (Maybe String) String
  , imageUpload : ImageUpload.Model
  , notification : Notification
  }

initial : Model
initial =
  { image = Field.required "Profile Picture"
  , imageUpload = ImageUpload.initial
  , notification = Notification.Empty
  }

reload : Model -> Model
reload model =
  { model
  | image = Field.reload model.image
  }

parse : Model -> Maybe Base64File
parse model =
  let
    result =
      Validation.andThen (Field.validate model.image) <| \image ->
      case model.imageUpload.name of
        Nothing -> Validation.invalid "An image must be uploaded."
        Just name ->
          Validation.valid
            { name = name
            , contents = image
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
  { request : Base64File -> Cmd (API.Response ())
  , finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update
  { initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse
  , request = config.request
  , finish = \_ -> config.finish
  }

type ModelMsg
  = SetImage (Croppie.Result ModelMsg)
  | ImageUploadMsg ImageUpload.Msg

type alias Msg = Form.Msg ModelMsg () ()

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
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
  , Input.button "Save" ()
  ]
