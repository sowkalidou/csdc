module CSDC.Component.Panel exposing
  ( Model
  , Item
  , initial
  , Msg (..)
  , update
  , view
  )

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
  Html.div
    [ Html.Attributes.class "box"
    , Html.Attributes.style "height" "100%"
    , Html.Attributes.style "overflow-y" "hidden"
    , Html.Attributes.style "padding-bottom" "85px"
    ]
    [ viewTitle model.name
    , Html.div
        [ Html.Attributes.style "height" "100%"
        , Html.Attributes.style "padding" "5px"
        , Html.Attributes.style "overflow-y" "auto"
        ]
        (List.map (viewItem model.selected) model.items)
    ]

viewTitle : String -> Html (Msg i)
viewTitle name =
  Html.h4
    [ Html.Attributes.class "title"
    ]
    [ Html.text name
    ]

viewItem : Maybe i -> Item i -> Html (Msg i)
viewItem selected {index, title, description} =
  Html.div
    [ if selected == Just index
      then Html.Attributes.class "box option-box has-background-grey-lighter is-shadowless"
      else Html.Attributes.class "box option-box has-background-white-ter is-shadowless"
    , Html.Events.onClick <| SetSelected (Just index)
    ]
    [ Html.strong [] [ Html.text title ]
    , Html.br [] []
    , Html.text description
    ]
