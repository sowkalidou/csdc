module CSDC.View.Explorer exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.UI.BoxImageText as BoxImageText
import CSDC.UI.Column as Column
import CSDC.UI.Modal as Modal
import CSDC.UI.PreviewImageText as PreviewImageText
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Types exposing (..)

import Browser.Dom as Dom
import Delay
import List
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { left : List (WithId Unit)
  , center : List (WithId Unit)
  , right : List (WithId Unit)
  , selected : Maybe (Id Unit)
  , isModalOpen : Bool
  , search : String
  , searchIsLoading : Bool
  , notification : Notification
  }

initial : Model
initial =
  { left = []
  , center = []
  , right = []
  , selected = Nothing
  , isModalOpen = False
  , search = ""
  , searchIsLoading = False
  , notification = Notification.Empty
  }

setup : Cmd Msg
setup = Cmd.map GetUserUnits API.getUserUnits

--------------------------------------------------------------------------------
-- Update

type Column
  = Left
  | Center
  | Right

type Msg
  = ViewUnit (Id Unit)
  | CloseModal
  | Reset
  | Focus
  | FocusResult (Result Dom.Error ())
  | GetUserUnits (API.Response (List (WithId Unit)))
  | GetUnitParents (API.Response (List UnitSubpart))
  | GetUnitChildren (API.Response (List UnitSubpart))
  | SelectUnit Column (Id Unit)
  | SearchInput String
  | SearchUnits (API.Response (List (WithId Unit)))

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  let
   onSuccess = Notification.withResponse Reset model
  in
  case msg of
    ViewUnit m ->
      ( { model | isModalOpen = False }
      , Page.goTo pageInfo (Page.Unit m)
      )

    CloseModal ->
      ( { model | isModalOpen = False }
      , Cmd.none
      )

    Reset ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

    Focus ->
      let
        calculate p e = e.element.y - p.element.y - p.element.height / 2 + e.element.height / 2
      in
      ( model
      , Task.map2 (\p e -> (p,e)) (Dom.getElement "Units-items") (Dom.getElement "selected-item")
        |> Task.andThen (\(p,e) -> Dom.setViewportOf "Units-items" 0 (calculate p e))
        |> Task.attempt FocusResult
      )

    FocusResult r ->
      ( model
      , case r of
          Err e -> Debug.log ("Could not focus: " ++ Debug.toString e) Cmd.none
          _ -> Cmd.none
      )

    GetUserUnits res -> onSuccess res <| \units ->
      ( { initial | center = units }
      , Cmd.none
      )

    SelectUnit component id ->
      case component of
        Center ->
          let
            selected = Just id
            isModalOpen = model.selected == selected
          in
            ( { model | selected = selected, isModalOpen = isModalOpen }
            , getUnitContext id
            )

        Left ->
          let
            selected = Just id
          in
            ( { model | left = [], center = model.left, selected = selected }
            , getUnitContext id
            )

        Right ->
          let
            selected = Just id
          in
            ( { model | right = [], center = model.right, selected = selected }
            , getUnitContext id
            )

    SearchInput input ->
      if String.length input == 0
      then
        ( { model | search = input, searchIsLoading = True }
        , setup
        )
      else if String.length input < 3
      then
        ( { model | search = input, searchIsLoading = False }
        , Cmd.none
        )
      else
        ( { model | search = input, searchIsLoading = True }
        , Cmd.map SearchUnits <| API.searchUnits input
        )

    GetUnitChildren res -> onSuccess res <| \units ->
      ( { model | right = List.map fromUnitSubpart units }
      , Cmd.none
      )

    GetUnitParents res -> onSuccess res <| \units ->
      ( { model | left = List.map fromUnitSubpart units }
      , Cmd.none
      )

    SearchUnits res -> onSuccess res <| \units ->
      ( { model
        | center = units
        , left = []
        , right = []
        , searchIsLoading = False
        }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  [ Html.h1
      [ Html.Attributes.class "title" ]
      [ Html.text "Explorer" ]
  , Html.div
      [ Html.Attributes.class "columns"
      , Html.Attributes.style "height" "100%"
      ]
      [ Html.div
          [ Html.Attributes.class "column is-one-third" ]
          [ Column.view "Parents" [] <|
            viewUnits Left model.selected model.left
          ]
      , Html.div
          [ Html.Attributes.class "column is-one-third" ]
          [ Column.view "Units" [viewSearch model.search] <|
            viewUnits Center model.selected model.center
          ]
      , Html.div
          [ Html.Attributes.class "column is-one-third" ]
          [ Column.view "Children" [] <|
            viewUnits Right model.selected model.right
          ]
      ]
  , Modal.view model.isModalOpen CloseModal <|
      case model.selected of
        Nothing -> Html.div [] []
        Just uid ->
          case lookupById uid model.center of
            Nothing ->
              Html.div [] []
            Just unit ->
              PreviewImageText.view unit.value (ViewUnit unit.id)
  ] ++
  Notification.view model.notification

--------------------------------------------------------------------------------
-- Helpers

viewUnits : Column -> Maybe (Id Unit) -> List (WithId Unit) -> List (Html Msg)
viewUnits column selected units =
  let
    toBox unit =
      let
        isSelected = column == Center && selected == Just unit.id
      in
        Html.map (SelectUnit column) <|
        BoxImageText.view isSelected unit.id unit.value
  in
    List.map toBox units

fromUnitSubpart : UnitSubpart -> WithId Unit
fromUnitSubpart unitSubpart =
  { id = unitSubpart.id
  , value = unitSubpart.unit
  }

getUnitContext : Id Unit -> Cmd Msg
getUnitContext id =
  Cmd.batch
    [ Cmd.map GetUnitChildren <| API.getUnitChildren id
    , Cmd.map GetUnitParents <| API.getUnitParents id
    ]

viewSearch : String -> Html Msg
viewSearch input =
  Html.input
    [ Html.Attributes.class "input"
    , Html.Attributes.type_ "text"
    , Html.Attributes.placeholder "Search"
    , Html.Attributes.value input
    , Html.Events.onInput SearchInput
    , Html.Attributes.style "width" "50%"
    , Html.Attributes.style "margin-top" "-10px"
    ]
    []

