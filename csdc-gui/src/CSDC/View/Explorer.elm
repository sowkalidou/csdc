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
import Task

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { left : List (WithId Unit)
  , center : List (WithId Unit)
  , right : List (WithId Unit)
  , notification : Notification
  , selected : Maybe (Id Unit)
  , isModalOpen : Bool
  }

initial : Model
initial =
  { left = []
  , center = []
  , right = []
  , notification = Notification.Empty
  , selected = Nothing
  , isModalOpen = False
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
      , Task.map2 (\p e -> (p,e)) (Dom.getElement "Units-items") (Dom.getElement "Units-selected-item")
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
      ( { model | center = units }
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

    GetUnitChildren res -> onSuccess res <| \units ->
      ( { model | right = List.map fromUnitSubpart units }
      , Cmd.none
      )

    GetUnitParents res -> onSuccess res <| \units ->
      ( { model | left = List.map fromUnitSubpart units }
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
          [ Column.view "Parents" [] (viewUnits Left model.selected model.left) ]
      , Html.div
          [ Html.Attributes.class "column is-one-third" ]
          [ Column.view "Units" [] (viewUnits Center model.selected model.center) ]
      , Html.div
          [ Html.Attributes.class "column is-one-third" ]
          [ Column.view "Children" [] (viewUnits Right model.selected model.right) ]
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

