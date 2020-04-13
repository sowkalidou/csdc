module CSDC.Component.Panel exposing
  ( Model
  , initial
  , Msg (..)
  , update
  , view
  )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Dict
import Dict exposing (Dict)
import List

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { name : String
  , items : Dict Int String
  , selected : Maybe Int
  }

initial : String -> Model
initial name =
  { name = name
  , items = Dict.empty
  , selected = Nothing
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = SetItems (Dict Int String)
  | SetSelected (Maybe Int)

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetItems items ->
      { model | items = items }

    SetSelected selected ->
      { model | selected = selected }

--------------------------------------------------------------------------------
-- View

view : Model -> Element Msg
view model =
  column
    [ height fill
    , width <| fillPortion 1
    , Border.width 1
    , Border.color <| rgb255 92 99 118
    , Border.rounded 5
    ]
    ( [ viewTitle model.name ] ++
      List.map (viewItem model.selected) (Dict.toList model.items)
    )

viewTitle : String -> Element Msg
viewTitle name =
  row
    [ height (px 50)
    , padding 10
    , width fill
    , Background.color <| rgb255 92 99 118
    , Font.size 24
    ]
    [ el [ centerX ] (text name)
    ]

viewItem : Maybe Int -> (Int, String) -> Element Msg
viewItem selected (this,name) =
  row
    [ height (px 30)
    , padding 10
    , width fill
    , Events.onClick <| SetSelected (Just this)
    , if selected == Just this
      then Background.color <| rgb255 142 151 164
      else Background.color <| rgb255 255 255 255
    ]
    [ text name
    ]
