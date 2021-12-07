module Main exposing (main)

import UI.Menu as Menu
import UI.Navbar as Navbar
import UI.Search as Search
import Notification exposing (Notification)
import Page as Page exposing (Page)
import Types exposing (..)
import Page.Explorer as Explorer
import Page.Studio as Studio
import Page.Person as Person
import Page.Unit as Unit

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes
import List
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
  , viewPerson : Person.Model
  , viewUnit : Unit.Model
  , explorer : Explorer.Model
  , studio : Studio.Model
  , search : Search.Model
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
      , explorer = Explorer.initial
      , studio = Studio.initial
      , viewPerson = Person.initial
      , viewUnit = Unit.initial
      , search = Search.initial
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
  | MenuMsg Menu.Msg
  | ExplorerMsg Explorer.Msg
  | PersonMsg Person.Msg
  | UnitMsg Unit.Msg
  | StudioMsg Studio.Msg
  | SearchMsg Search.Msg

routeCmd : Page -> Cmd Msg
routeCmd page =
  case page of
    Page.Studio ->
      Cmd.map StudioMsg Studio.setup
    Page.Explorer ->
      Cmd.map ExplorerMsg Explorer.setup
    Page.Unit tab uid ->
      Cmd.map UnitMsg (Unit.setup uid tab)
    Page.Person uid ->
      Cmd.map PersonMsg (Person.setup uid)

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

    SearchMsg m ->
      let
        (search, cmd) = Search.update pageInfo m model.search
      in
        ( { model | search = search }
        , Cmd.map SearchMsg cmd
        )

--------------------------------------------------------------------------------
-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

--------------------------------------------------------------------------------
-- View

view : Model -> Browser.Document Msg
view model =
  { title = "DAO"
  , body =
      [ Navbar.view <| Html.map SearchMsg <| Search.view model.search
      , Html.div
          [ Html.Attributes.class "columns"
          , Html.Attributes.attribute "style" "padding:25px; height: calc(100vh - 80px)"
          ]
          [ Html.div
              [ Html.Attributes.class "column is-one-fifth"
              ]
              [ viewMenu model
              ]
          , Html.div
              [ Html.Attributes.class "column is-four-fifths"
              ]
              [ Html.div
                  [ Html.Attributes.class "content"
                  , Html.Attributes.style "height" "100%"
                  ]
                  (viewMain model)
              ]
          ]
      ]
  }

viewMenu : Model -> Html Msg
viewMenu model =
  Html.map MenuMsg <| Menu.view (Menu.fromPage model.page)

viewMain : Model -> List (Html Msg)
viewMain model =
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

    Page.Unit _ _ ->
      List.map (Html.map UnitMsg) <|
      Unit.view model.viewUnit
