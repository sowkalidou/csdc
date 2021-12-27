module Form.MailInvitation exposing
  ( Model
  , initial
  , Msg
  , updateWith
  , view
  )

import API as API
import Notification exposing (Notification)
import Types exposing (..)
import Input as Input
import Field exposing (Field)
import Form
import Page

import Html exposing (Html)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { invitees : Field String (List String)
  , message : Field String String
  , notification : Notification
  }

initial : Model
initial =
  { invitees = Field.emailList "Invitees"
  , message = Field.requiredString "Message"
  , notification = Notification.Empty
  }

reload : Model -> Model
reload model =
  { model
  | invitees = Field.reload model.invitees
  , message = Field.reload model.message
  }

parse : Model -> Maybe MailInvitation
parse model = Result.toMaybe <|
  Field.with model.invitees <| \invitees ->
  Field.with model.message <| \message ->
  Ok
    { invitees = invitees
    , message = message
    }

--------------------------------------------------------------------------------
-- Update

type alias Config =
  { unit : Id Unit
  , pageInfo : Page.Info
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update
  { initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse
  , request = API.sendMailInvitation config.unit
  , finish = \_ -> Cmd.none
  , pageInfo = config.pageInfo
  }

type ModelMsg
  = SetInvitees String
  | SetMessage String

type alias Msg = Form.Msg ModelMsg () ()

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetInvitees val ->
      ( { model | invitees = Field.set val model.invitees }
      , Cmd.none
      )

    SetMessage val ->
      ( { model | message = Field.set val model.message }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  [ Input.text model.invitees SetInvitees
  , Input.textarea model.message SetMessage
  , Input.button "Send Invitation" ()
  ]
