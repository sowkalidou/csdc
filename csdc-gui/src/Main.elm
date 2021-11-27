module Main exposing (..)

import CSDC.API as API
import CSDC.Component.Menu as Menu
import CSDC.Component.Navbar as Navbar
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Page exposing (Page)
import CSDC.Types exposing (..)
import CSDC.View.Admin as Admin
import CSDC.View.Explorer as Explorer
import CSDC.View.InvitationMember as InvitationMember
import CSDC.View.Studio as Studio
import CSDC.View.Person as Person
import CSDC.View.Unit as Unit
import CSDC.View.UnitAdmin as UnitAdmin

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import List
import Maybe
import Maybe exposing (withDefault)
import String
import Url

--------------------------------------------------------------------------------
-- Main

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { key: Nav.Key
  , url: Url.Url
  , page : Page
  , info : Maybe PersonInfo
  , admin : Admin.Model
  , viewPerson : Person.Model
  , viewUnit : Unit.Model
  , viewUnitAdmin : UnitAdmin.Model
  , invitationMember : InvitationMember.Model
  , explorer : Explorer.Model
  , studio : Studio.Model
  , notification : Notification
  }

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  let
    (page, cmd) = route { key = key, url = url }
  in
    ( { key = key
      , url = url
      , page = page
      , info = Nothing
      , explorer = Explorer.initial
      , admin = Admin.initial
      , studio = Studio.initial
      , viewPerson = Person.initial
      , viewUnit = Unit.initial
      , viewUnitAdmin = UnitAdmin.initial
      , invitationMember = InvitationMember.initial
      , notification = Notification.Empty
      }
    , case page of
        Page.Studio -> cmd
        _ -> Cmd.batch [ routeCmd Page.Studio, cmd ]
    )

--------------------------------------------------------------------------------
-- Update

type Msg
  = UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest
  | AdminMsg Admin.Msg
  | MenuMsg Menu.Msg
  | ExplorerMsg Explorer.Msg
  | PersonMsg Person.Msg
  | UnitMsg Unit.Msg
  | UnitAdminMsg UnitAdmin.Msg
  | InvitationMemberMsg InvitationMember.Param InvitationMember.Msg
  | StudioMsg Studio.Msg
  | APIMsg API.Msg

routeCmd : Page -> Cmd Msg
routeCmd page =
  case page of
    Page.Studio ->
      Cmd.map APIMsg API.rootPerson
    Page.Explorer ->
      Cmd.map ExplorerMsg Explorer.setup
    Page.Unit uid ->
      Cmd.map UnitMsg (Unit.setup uid)
    Page.UnitAdmin uid ->
      Cmd.map UnitAdminMsg (UnitAdmin.setup uid)
    Page.Person uid ->
      Cmd.map PersonMsg (Person.setup uid)
    Page.InvitationMember pid ->
      Cmd.none
    Page.Admin ->
      Cmd.none

route : Page.Info -> (Page, Cmd Msg)
route info =
  case info.url.fragment of
    Nothing ->
      (Page.Studio, Page.goTo info Page.Studio)
    Just fragment ->
      let
        page = Page.fromFragment fragment
      in
        (page, routeCmd page)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    pageInfo = { key = model.key, url = model.url }
  in
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      let
        (page, cmd) = route { key = model.key, url = url }
      in
        ( { model | page = page, url = url }, cmd )

    AdminMsg m ->
      let
        (admin, cmd) = Admin.update m model.admin
      in
        ( { model | admin = admin }
        , Cmd.map AdminMsg cmd
        )

    MenuMsg (Menu.SetItem menu) ->
      let
        page = Menu.toPage menu
      in
        ( { model | page = page }
        , Page.goTo pageInfo page
        )

    ExplorerMsg m ->
      let
        (explorer, cmd) = Explorer.update pageInfo m model.explorer
      in
        ( { model | explorer = explorer }
        , Cmd.map ExplorerMsg cmd
        )

    StudioMsg m ->
      let
        (studio, cmd) = Studio.update pageInfo m model.studio
      in
        ( { model | studio = studio }
        , Cmd.map StudioMsg cmd
        )

    PersonMsg m ->
      let
        (viewPerson, cmd) = Person.update pageInfo m model.viewPerson
      in
        ( { model | viewPerson = viewPerson }
        , Cmd.map PersonMsg cmd
        )

    UnitMsg m ->
      let
        (viewUnit, cmd) = Unit.update pageInfo m model.viewUnit
      in
        ( { model | viewUnit = viewUnit }
        , Cmd.map UnitMsg cmd
        )

    UnitAdminMsg m ->
      let
        (viewUnitAdmin, cmd) = UnitAdmin.update pageInfo m model.viewUnitAdmin
      in
        ( { model | viewUnitAdmin = viewUnitAdmin }
        , Cmd.map UnitAdminMsg cmd
        )

    InvitationMemberMsg p m ->
      let
        (invitationMember, cmd) = InvitationMember.update pageInfo m p model.invitationMember
      in
        ( { model | invitationMember = invitationMember }
        , Cmd.map (InvitationMemberMsg p) cmd
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
              ( model
              , Cmd.batch
                  [ Cmd.map StudioMsg <| Studio.setup id
                  , Cmd.map APIMsg <| API.getPersonInfo id
                  ]
              )

        API.GetPersonInfo result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok info ->
              ( { model | info = Just info }
              , Cmd.none
              )


        _ -> (model, Cmd.none)

--------------------------------------------------------------------------------
-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

--------------------------------------------------------------------------------
-- View

view : Model -> Browser.Document Msg
view model =
  { title = "CSDC DAO"
  , body =
      [ Navbar.view
      , Html.div
          [ Html.Attributes.class "columns"
          , Html.Attributes.attribute "style" "padding:25px; height: calc(100vh - 80px)"
          ]
          [ Html.div
              [ Html.Attributes.class "column is-one-fifth"
              ]
              [ menuPanel model
              ]
          , Html.div
              [ Html.Attributes.class "column is-four-fifths"
              ]
              [ Html.div
                  [ Html.Attributes.class "content"
                  , Html.Attributes.style "height" "100%"
                  ]
                  (mainPanel model)
              ]
          ]
      ]
  }

menuPanel : Model -> Html Msg
menuPanel model =
  Html.map MenuMsg <| Menu.view (Menu.fromPage model.page)

mainPanel : Model -> List (Html Msg)
mainPanel model =
  let
    wrapElements elements =
      [ layout [] <|
        column
          [ height fill
          , width <| fillPortion 5
          , spacing 10
          , padding 10
          ]
          elements
      ]
  in
    case model.page of
      Page.Studio ->
        List.map (Html.map StudioMsg) <|
        Studio.view model.studio

      Page.Explorer ->
        List.map (Html.map ExplorerMsg) <|
        Explorer.view model.explorer

      Page.Person _ ->
        List.map (Html.map PersonMsg) <|
        Person.view model.viewPerson

      Page.Unit _ ->
        List.map (Html.map UnitMsg) <|
        Unit.view model.info model.viewUnit

      Page.UnitAdmin _ ->
        List.map (Html.map UnitAdminMsg) <|
        UnitAdmin.view model.info model.viewUnitAdmin

      Page.Admin ->
        wrapElements <|
        List.map (Element.map AdminMsg) <|
        Admin.view model.admin

      Page.InvitationMember pid ->
        wrapElements <|
        case model.info of
          Just person ->
            let
              param = {person = pid, user = person}
              toMsg = InvitationMemberMsg param
            in
              List.map (Element.map toMsg) <|
              [ InvitationMember.view param model.invitationMember ]
          _ -> []
