module CSDC.View.Explorer exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Modal as Modal
import CSDC.Component.Panel as Panel
import CSDC.View.UnitPreview as UnitPreview
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
  { left : Panel.Model (Id Unit)
  , center : Panel.Model (Id Unit)
  , right : Panel.Model (Id Unit)
  , notification : Notification
  , selected : Maybe (Id Unit)
  , preview : Maybe (WithId Unit)
  }

initial : Model
initial =
  { left = Panel.initial "Parents"
  , center = Panel.initial "Units"
  , right = Panel.initial "Children"
  , notification = Notification.Empty
  , selected = Nothing
  , preview = Nothing
  }

setup : Cmd Msg
setup = Cmd.map GetUserUnits API.getUserUnits

--------------------------------------------------------------------------------
-- Update

type Component
  = Left
  | Center
  | Right

type Msg
  = LeftMsg (Panel.Msg (Id Unit))
  | CenterMsg (Panel.Msg (Id Unit))
  | RightMsg (Panel.Msg (Id Unit))
  | Unit (Id Unit)
  | CloseModal
  | Reset
  | Focus
  | FocusResult (Result Dom.Error ())
  | GetUserUnits (API.Response (List (WithId Unit)))
  | GetUnitParents (API.Response (List UnitSubpart))
  | GetUnitChildren (API.Response (List UnitSubpart))
  | SelectUnit Component (Id Unit) (API.Response Unit)

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  let
   onSuccess = Notification.withResponse Reset model
  in
  case msg of
    CenterMsg m ->
      ( { model | center = Panel.update m model.center }
      , case m of
          Panel.SetSelected (Just id) ->
            Cmd.map (SelectUnit Center id) (API.selectUnit id)
          _ ->
            Cmd.none
      )

    LeftMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | left =
                Panel.update
                  (Panel.SetItems []) model.left

            , center =
                Panel.update
                  (Panel.SetItems model.left.items)
                  model.center

            , right =
                Panel.update
                  (Panel.SetItems model.center.items)
                  model.right
            }
          , Cmd.map (SelectUnit Left id) (API.selectUnit id)
          )

        _ ->
          ( model
          , Cmd.none
          )

    RightMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | left =
                Panel.update
                  (Panel.SetItems model.center.items)
                  model.left
            , center =
                Panel.update
                  (Panel.SetItems model.right.items)
                  model.center
            , right =
                Panel.update
                  (Panel.SetItems []) model.right
            }
          , Cmd.batch
              [ Cmd.map (SelectUnit Right id) (API.selectUnit id)
              , Delay.after 0.1 Delay.Second Focus
              ]
          )

        _ ->
          ( model
          , Cmd.none
          )

    Unit m ->
      ( { model | preview = Nothing }
      , Page.goTo pageInfo (Page.Unit m)
      )

    CloseModal ->
      ( { model | preview = Nothing }
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
      let
        toItem w =
          { index = w.id
          , title = w.value.name
          , description = w.value.description
          }
        dict = List.map toItem units
        center = Panel.update (Panel.SetItems dict) model.center
      in
        ( { model | center = center }
        , Cmd.none
        )

    SelectUnit component id res -> onSuccess res <| \unit ->
      case component of
        -- When the message comes from the center:
        --
        -- - change selected
        -- - display preview
        -- - launch commands for parents and children
        --
        Center ->
          let
            center =
              Panel.update (Panel.SetSelected (Just id)) model.center

            selected = Just id

            -- The preview only shows after clicking a second time in the
            -- same unit, after it is selected
            preview =
              if model.selected == Just id
              then Just { id = id, value = unit }
              else Nothing
          in
            ( { model | center = center, selected = selected, preview = preview }
            , Cmd.batch
                [ Cmd.map GetUnitChildren <| API.getUnitChildren id
                , Cmd.map GetUnitParents <| API.getUnitParents id
                ]
            )

        Left ->
          let
            center =
              Panel.update (Panel.SetSelected (Just id)) model.center

            selected = Just id
          in
            ( { model | center = center, selected = selected }
            , Cmd.map GetUnitParents <| API.getUnitParents id
            )

        Right ->
          let
            center =
              Panel.update (Panel.SetSelected (Just id)) model.center

            selected = Just id
          in
            ( { model | center = center, selected = selected }
            , Cmd.map GetUnitChildren <| API.getUnitChildren id
            )

    GetUnitChildren res -> onSuccess res <| \units ->
      let
        pairs = toItems units
        right = Panel.update (Panel.SetItems pairs) model.right
      in
        ( { model | right = right }
        , Cmd.none
        )

    GetUnitParents res -> onSuccess res <| \units ->
      let
        pairs = toItems units
        left = Panel.update (Panel.SetItems pairs) model.left
      in
        ( { model | left = left }
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
          [ Html.map LeftMsg <| Panel.view model.left ]
      , Html.div
          [ Html.Attributes.class "column is-one-third" ]
          [ Html.map CenterMsg <| Panel.view model.center ]
      , Html.div
          [ Html.Attributes.class "column is-one-third" ]
          [ Html.map RightMsg <| Panel.view model.right ]
      ]
  , case model.preview of
      Nothing ->
        Html.div [] []
      Just unit ->
        Modal.view True CloseModal <|
          UnitPreview.view unit.value (Unit unit.id)
  ] ++
  Notification.view model.notification

--------------------------------------------------------------------------------
-- Helpers

toItems : List UnitSubpart -> List (Panel.Item (Id Unit))
toItems =
  let
    toItem unitSubpart =
      { index = unitSubpart.id
      , title = unitSubpart.unit.name
      , description = unitSubpart.unit.description
      }
  in
    List.map toItem
