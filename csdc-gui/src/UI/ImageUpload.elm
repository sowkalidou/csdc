port module UI.ImageUpload exposing (..)

import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task

import Croppie
import Croppie.Events
import Croppie.Options
import Croppie.BindOptions
import Croppie.ResultOptions

port croppie : Croppie.Data -> Cmd msg

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { image : Maybe String
  , name : Maybe String
  }

initial : Model
initial =
  { image = Nothing
  , name = Nothing
  }

defaultImage : String
defaultImage = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAQAAABIkb+zAAAAZUlEQVR42u3PMQ0AAAgDMOZfLzf8OCBpHTQ99VoEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBK4FS7q74S8Gh5UAAAAASUVORK5CYII="

setup : Maybe String -> Cmd msg
setup mimage = croppie <| Croppie.bind "profile-picture"
  [ Croppie.BindOptions.url <|
    case mimage of
      Nothing -> defaultImage
      Just image -> image
  ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = Pick
  | GotFile File
  | GotUrl String
  | GetResult

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pick ->
      ( model
      , Select.files ["image/*"] (\file _ -> GotFile file)
      )

    GotFile file ->
      ( { model | name = Just <| File.name file }
      , Task.perform GotUrl <| File.toUrl file
      )

    GotUrl url ->
      ( { model | image = Just url }
      , croppie <|
          Croppie.bind "profile-picture"
            [ Croppie.BindOptions.url url ]
      )

    GetResult ->
      ( model
      , croppie <|
          Croppie.result "profile-picture"
            [ Croppie.ResultOptions.type_ Croppie.ResultOptions.Base64 ]
      )

--------------------------------------------------------------------------------
-- View

view : Model -> (Msg -> msg) -> (Croppie.Result msg -> msg) -> Html msg
view _ makeMsg gotResult =
  Html.div
    [ Html.Attributes.class "box has-text-centered"
    , Html.Attributes.style "height" "240px"
    , Html.Attributes.style "width" "200px"
    , Html.Attributes.style "margin" "auto"
    , Html.Events.onMouseLeave (makeMsg GetResult)
    ]
    [ Html.div
        [ Html.Attributes.style "height" "160px"
        , Html.Attributes.style "width" "160px"
        ]
        [ Croppie.croppie
            [ Croppie.Options.viewport
                { width = 96
                , height = 96
                , type_ = Croppie.Options.Square
                }
            , Croppie.Options.boundary
                { width = 96
                , height = 96
                }
            ]
            [ Html.Attributes.id "profile-picture"
            , Croppie.Events.onResult gotResult
            ]
        ]
    , Html.button
        [ Html.Attributes.class "button"
        , Html.Attributes.style "margin-bottom" "10px"
        , Html.Events.onClick (makeMsg Pick)
        ]
        [ Html.text "Upload Image"
        ]
    ]
