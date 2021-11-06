module Main exposing (..)

import CSDC.API as API
import CSDC.Component.Admin as Admin
import CSDC.Component.Explorer as Explorer
import CSDC.Component.InvitationMember as InvitationMember
import CSDC.Component.Menu as Menu
import CSDC.Component.MessageMember as MessageMember
import CSDC.Component.MessageSubpart as MessageSubpart
import CSDC.Component.PreviewMessage as PreviewMessage
import CSDC.Component.PreviewReply as PreviewReply
import CSDC.Component.ReplyMember as ReplyMember
import CSDC.Component.ReplySubpart as ReplySubpart
import CSDC.Component.Studio as Studio
import CSDC.Component.ViewPerson as ViewPerson
import CSDC.Component.ViewUnit as ViewUnit
import CSDC.Component.ViewUnitAdmin as ViewUnitAdmin
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Page exposing (Page)
import CSDC.Types exposing (..)

import Browser
import Browser.Navigation as Nav
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
  , info : Maybe (User PersonInfo)
  , admin : Admin.Model
  , viewPerson : ViewPerson.Model
  , viewUnit : ViewUnit.Model
  , viewUnitAdmin : ViewUnitAdmin.Model
  , messageMember : MessageMember.Model
  , invitationMember : InvitationMember.Model
  , replyMember : ReplyMember.Model
  , messageSubpart : MessageSubpart.Model
  , replySubpart : ReplySubpart.Model
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
      , viewPerson = ViewPerson.initial
      , viewUnit = ViewUnit.initial
      , viewUnitAdmin = ViewUnitAdmin.initial
      , messageMember = MessageMember.initial
      , invitationMember = InvitationMember.initial
      , replyMember = ReplyMember.initial
      , messageSubpart = MessageSubpart.initial
      , replySubpart = ReplySubpart.initial
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
  | ViewPersonMsg ViewPerson.Msg
  | ViewUnitMsg ViewUnit.Msg
  | ViewUnitAdminMsg ViewUnitAdmin.Msg
  | MessageMemberMsg MessageMember.Param MessageMember.Msg
  | InvitationMemberMsg InvitationMember.Param InvitationMember.Msg
  | ReplyMemberMsg ReplyMember.Param ReplyMember.Msg
  | MessageSubpartMsg MessageSubpart.Param MessageSubpart.Msg
  | ReplySubpartMsg ReplySubpart.Param ReplySubpart.Msg
  | StudioMsg Studio.Msg
  | APIMsg API.Msg

routeCmd : Page -> Cmd Msg
routeCmd page =
  case page of
    Page.Studio ->
      Cmd.map APIMsg API.rootPerson
    Page.Explorer ->
      Cmd.map ExplorerMsg Explorer.setup
    Page.ViewUnit uid ->
      Cmd.map ViewUnitMsg (ViewUnit.setup uid)
    Page.ViewUnitAdmin uid ->
      Cmd.map ViewUnitAdminMsg (ViewUnitAdmin.setup uid)
    Page.ViewPerson uid ->
      Cmd.map ViewPersonMsg (ViewPerson.setup uid)
    Page.InvitationMember pid ->
      Cmd.none
    Page.MessageMember pid uid mtype ->
      Cmd.map (MessageMemberMsg { messageType = mtype }) (MessageMember.setup pid uid)
    Page.ReplyMember _ _ ->
      Cmd.none
    Page.MessageSubpart pid uid mtype ->
      Cmd.map (MessageSubpartMsg { messageType = mtype }) (MessageSubpart.setup pid uid)
    Page.ReplySubpart _ _ ->
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

    ViewPersonMsg m ->
      let
        (viewPerson, cmd) = ViewPerson.update pageInfo m model.viewPerson
      in
        ( { model | viewPerson = viewPerson }
        , Cmd.map ViewPersonMsg cmd
        )

    ViewUnitMsg m ->
      let
        (viewUnit, cmd) = ViewUnit.update pageInfo m model.viewUnit
      in
        ( { model | viewUnit = viewUnit }
        , Cmd.map ViewUnitMsg cmd
        )

    ViewUnitAdminMsg m ->
      let
        (viewUnitAdmin, cmd) = ViewUnitAdmin.update pageInfo m model.viewUnitAdmin
      in
        ( { model | viewUnitAdmin = viewUnitAdmin }
        , Cmd.map ViewUnitAdminMsg cmd
        )

    MessageMemberMsg p m ->
      let
        (messageMember, cmd) = MessageMember.update pageInfo m p model.messageMember
      in
        ( { model | messageMember = messageMember }
        , Cmd.map (MessageMemberMsg p) cmd
        )

    InvitationMemberMsg p m ->
      let
        (invitationMember, cmd) = InvitationMember.update pageInfo m p model.invitationMember
      in
        ( { model | invitationMember = invitationMember }
        , Cmd.map (InvitationMemberMsg p) cmd
        )

    ReplyMemberMsg p m ->
      let
        (replyMember, cmd) = ReplyMember.update pageInfo m p model.replyMember
      in
        ( { model | replyMember = replyMember }
        , Cmd.map (ReplyMemberMsg p) cmd
        )

    MessageSubpartMsg p m ->
      let
        (messageSubpart, cmd) = MessageSubpart.update pageInfo m p model.messageSubpart
      in
        ( { model | messageSubpart = messageSubpart }
        , Cmd.map (MessageSubpartMsg p) cmd
        )

    ReplySubpartMsg p m ->
      let
        (replySubpart, cmd) = ReplySubpart.update pageInfo m p model.replySubpart
      in
        ( { model | replySubpart = replySubpart }
        , Cmd.map (ReplySubpartMsg p) cmd
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
              case id of
                Admin ->
                  ( { model | info = Just Admin }
                  , Cmd.none
                  )
                User pid ->
                  ( model
                  , Cmd.batch
                      [ Cmd.map StudioMsg <| Studio.setup pid
                      , Cmd.map APIMsg <| API.getPersonInfo pid
                      ]
                  )

        API.GetPersonInfo result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok info ->
              ( { model | info = Just (User info) }
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
      [ layout [] <|
        row [ height fill, width fill ]
            [ menuPanel model
            , mainPanel model
            ]
      ]
  }

menuPanel : Model -> Element Msg
menuPanel model =
  Element.map MenuMsg <| Menu.view (Menu.fromPage model.page)

mainPanel : Model -> Element Msg
mainPanel model =
  column
    [ height fill
    , width <| fillPortion 5
    , spacing 10
    , padding 10
    ] <|
    case model.page of
      Page.Studio ->
        List.map (Element.map StudioMsg) <|
        Studio.view model.studio

      Page.Explorer ->
        List.map (Element.map ExplorerMsg) <|
        Explorer.view model.explorer

      Page.ViewPerson _ ->
        List.map (Element.map ViewPersonMsg) <|
        ViewPerson.view model.viewPerson

      Page.ViewUnit _ ->
        List.map (Element.map ViewUnitMsg) <|
        ViewUnit.view model.info model.viewUnit

      Page.ViewUnitAdmin _ ->
        List.map (Element.map ViewUnitAdminMsg) <|
        ViewUnitAdmin.view model.info model.viewUnitAdmin

      Page.Admin ->
        List.map (Element.map AdminMsg) <|
        Admin.view model.admin

      Page.InvitationMember pid ->
        case model.info of
          Just (User person) ->
            let
              param = {person = pid, user = person}
              toMsg = InvitationMemberMsg param
            in
              List.map (Element.map toMsg) <|
              [ InvitationMember.view param model.invitationMember ]
          _ -> []

      Page.MessageMember _ _ mtype ->
        let
          param = { messageType = mtype }
          toMsg = MessageMemberMsg param
        in
          List.map (Element.map toMsg) <|
          [ MessageMember.view param model.messageMember ]

      Page.ReplyMember mid mtype ->
        let
          param = { message = mid, messageType = mtype }
          toMsg = ReplyMemberMsg param
        in
          List.map (Element.map toMsg) <|
          [ ReplyMember.view param model.replyMember ]

      Page.MessageSubpart _ _ mtype ->
        let
          param = { messageType = mtype }
          toMsg = MessageSubpartMsg param
        in
          List.map (Element.map toMsg) <|
          [ MessageSubpart.view param model.messageSubpart ]

      Page.ReplySubpart mid mtype ->
        let
          param = {message = mid, messageType = mtype}
          toMsg = ReplySubpartMsg param
        in
          List.map (Element.map toMsg) <|
          [ ReplySubpart.view param model.replySubpart ]
