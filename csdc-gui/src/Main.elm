module Main exposing (..)

import CSDC.API as API
import CSDC.Component.Explorer as Explorer
import CSDC.Component.Menu as Menu
import CSDC.Component.NewPerson as NewPerson
import CSDC.Component.NewUnit as NewUnit
import CSDC.Component.Studio as Studio
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Input as Input
import Html exposing (Html)
import List
import Maybe
import Maybe exposing (withDefault)
import String

--------------------------------------------------------------------------------
-- Main

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { id : Maybe UserId
  , menu : Menu.Model
  , newPerson : NewPerson.Model
  , newUnit : NewUnit.Model
  , explorer : Explorer.Model
  , studio : Studio.Model
  , notification : Notification
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    (explorer, explorerCmd) = Explorer.initial ()
  in
    ( { id = Nothing
      , menu = Menu.initial
      , explorer = explorer
      , newPerson = NewPerson.initial
      , newUnit = NewUnit.initial
      , studio = Studio.initial
      , notification = Notification.Empty
      }
    , Cmd.batch
        [ Cmd.map ExplorerMsg explorerCmd
        , Cmd.map APIMsg API.rootPerson
        ]
    )

--------------------------------------------------------------------------------
-- Update

type Msg
  = NewPersonMsg NewPerson.Msg
  | NewUnitMsg NewUnit.Msg
  | MenuMsg Menu.Msg
  | ExplorerMsg Explorer.Msg
  | StudioMsg Studio.Msg
  | APIMsg API.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewUnitMsg m ->
      let
        (newUnit, cmd) = NewUnit.update m model.newUnit
      in
        ( { model | newUnit = newUnit }
        , Cmd.map NewUnitMsg cmd
        )

    NewPersonMsg m ->
      let
        (newPerson, cmd) = NewPerson.update m model.newPerson
      in
        ( { model | newPerson = newPerson }
        , Cmd.map NewPersonMsg cmd
        )

    MenuMsg m ->
      let
        menu = Menu.update m model.menu
      in
        ( { model | menu = menu }
        , Cmd.none
        )

    ExplorerMsg m ->
      let
        (explorer, cmd) = Explorer.update m model.explorer
      in
        ( { model | explorer = explorer }
        , Cmd.map ExplorerMsg cmd
        )

    StudioMsg m ->
      let
        (studio, cmd) = Studio.update m model.studio
      in
        ( { model | studio = studio }
        , Cmd.map StudioMsg cmd
        )

    APIMsg m ->
      case m of
        API.RootPerson result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok id ->
              ( { model | id = Just id }
              , case id of
                  Admin ->
                    Cmd.none
                  User pid ->
                    Cmd.map StudioMsg <| Studio.setup pid
              )

        _ -> (model, Cmd.none)

--------------------------------------------------------------------------------
-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

--------------------------------------------------------------------------------
-- View

view : Model -> Html Msg
view model =
  layout [] <|
    row [ height fill, width fill ]
        [ menuPanel model
        , mainPanel model
        ]

menuPanel : Model -> Element Msg
menuPanel model =
  Element.map MenuMsg <| Menu.view model.menu

mainPanel : Model -> Element Msg
mainPanel model =
  column
    [ height fill
    , width <| fillPortion 5
    , spacing 10
    , padding 10
    ] <|
    case model.menu of
      Menu.NewPerson ->
        [ Element.map NewPersonMsg <| NewPerson.view model.newPerson
        ]

      Menu.NewUnit ->
        [ Element.map NewUnitMsg <| NewUnit.view model.newUnit
        ]

      Menu.Explorer ->
        List.map (Element.map ExplorerMsg) <|
        Explorer.view model.explorer

      Menu.Studio ->
        List.map (Element.map StudioMsg) <|
        Studio.view model.studio
