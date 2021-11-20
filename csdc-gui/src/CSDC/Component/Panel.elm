module CSDC.Component.Panel exposing
  ( Model
  , Item
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.Component.Column as Column

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Dict
import Dict exposing (Dict)
import List

--------------------------------------------------------------------------------
-- Model

type alias Model i =
  { name : String
  , items : List (Item i)
  , selected : Maybe i
  }

type alias Item i =
  { index : i
  , title : String
  , description : String
  }

initial : String -> Model i
initial name =
  { name = name
  , items = []
  , selected = Nothing
  }

--------------------------------------------------------------------------------
-- Update

type Msg i
  = SetItems (List (Item i))
  | SetSelected (Maybe i)

update : Msg i -> Model i -> Model i
update msg model =
  case msg of
    SetItems items ->
      { model | items = items }

    SetSelected selected ->
      { model | selected = selected }

--------------------------------------------------------------------------------
-- View

view : Model i -> Html (Msg i)
view model =
  Column.make
    model.name
    []
    (List.map (viewItem model.name model.selected) model.items)

viewItem : String -> Maybe i -> Item i -> Html (Msg i)
viewItem name selected {index, title, description} =
  Html.div
    [ if selected == Just index
      then Html.Attributes.class "box option-box has-background-grey-lighter is-shadowless"
      else Html.Attributes.class "box option-box has-background-white-ter is-shadowless"
    , if selected == Just index
      then Html.Attributes.id (name ++ "-selected-item")
      else Html.Attributes.id (name ++ "unselected-item")
    , Html.Attributes.style "height" "88px"
    , Html.Attributes.style "overflow" "hidden"
    , Html.Attributes.style "text-overflow" "ellipsis"
    , Html.Attributes.style "white-space" "nowrap"
    , Html.Events.onClick <| SetSelected (Just index)
    ]
    [ Html.strong [] [ Html.text title ]
    , Html.br [] []
    , Html.text description
    ]
