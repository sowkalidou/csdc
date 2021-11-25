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
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Types exposing (..)

import Browser.Dom as Dom
import Delay
import Dict
import List
import Html exposing (Html)
import Html.Attributes
import Html.Events
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
setup = Cmd.map (APIMsg Root) API.rootUnit

--------------------------------------------------------------------------------
-- Update

type Component
  = Left
  | Center
  | Right
  | Root

type Msg
  = LeftMsg (Panel.Msg (Id Unit))
  | CenterMsg (Panel.Msg (Id Unit))
  | RightMsg (Panel.Msg (Id Unit))
  | APIMsg Component API.Msg
  | Unit (Id Unit)
  | CloseModal
  | Reset
  | Focus
  | FocusResult (Result Dom.Error ())

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  case msg of
    CenterMsg m ->
      ( { model | center = Panel.update m model.center }
      , case m of
          Panel.SetSelected (Just id) ->
            Cmd.map (APIMsg Center) (API.selectUnit id)
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
          , Cmd.map (APIMsg Left) (API.selectUnit id)
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
              [ Cmd.map (APIMsg Right) (API.selectUnit id)
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

    APIMsg component m ->
      let onSuccess = Notification.withResponse Reset model in
      case m of
        API.RootUnit res -> onSuccess res <| \id ->
          case model.selected of
            Nothing ->
              ( model
              , Cmd.map (APIMsg Root) <| API.selectUnit id
              )
            Just _ ->
              ( model, Cmd.none )

        API.SelectUnit id res -> onSuccess res <| \unit ->
          case component of
            Root ->
              let
                dict =
                  [ { index = id
                    , title = unit.name
                    , description = unit.description
                    }
                  ]
                center = Panel.update (Panel.SetItems dict) model.center
              in
                ( { model | center = center }
                , Cmd.none
                )

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
                , Cmd.map (APIMsg Center) <|
                  Cmd.batch
                    [ API.getUnitChildren id
                    , API.getUnitParents id
                    ]
                )

            Left ->
              let
                center =
                  Panel.update (Panel.SetSelected (Just id)) model.center

                selected = Just id
              in
                ( { model | center = center, selected = selected }
                , Cmd.map (APIMsg Center) <| API.getUnitParents id
                )

            Right ->
              let
                center =
                  Panel.update (Panel.SetSelected (Just id)) model.center

                selected = Just id
              in
                ( { model | center = center, selected = selected }
                , Cmd.map (APIMsg Center) <| API.getUnitChildren id
                )

        API.GetUnitChildren res -> onSuccess res <| \units ->
          let
            pairs = toItems units
            right = Panel.update (Panel.SetItems pairs) model.right
          in
            ( { model | right = right }
            , Cmd.none
            )

        API.GetUnitParents res -> onSuccess res <| \units ->
          let
            pairs = toItems units
            left = Panel.update (Panel.SetItems pairs) model.left
          in
            ( { model | left = left }
            , Cmd.none
            )

        _ ->
          ( model
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

toItems : IdMap a (WithId Unit) -> List (Panel.Item (Id Unit))
toItems =
  let
    toPair (_, withId) =
      { index = withId.id
      , title = withId.value.name
      , description = withId.value.description
      }
  in
    List.map toPair << idMapToList
