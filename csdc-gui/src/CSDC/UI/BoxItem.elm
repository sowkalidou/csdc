module CSDC.UI.BoxItem exposing
  ( Model
  , Size (..)
  , view
  )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import List

--------------------------------------------------------------------------------
-- Item

type Size = Small | Big

type alias Model msg =
  { id : String
  , contents : List (Html msg)
  , onClick : Maybe msg
  , selected : Bool
  , size : Size
  }

view : Model msg -> Html msg
view model =
  Html.div
    ( [ if model.selected
        then Html.Attributes.class "box option-box has-background-grey-lighter is-shadowless"
        else Html.Attributes.class "box option-box has-background-white-ter is-shadowless"
      , if model.selected
        then Html.Attributes.id (model.id ++ "-selected-item")
        else Html.Attributes.id (model.id ++ "-unselected-item")
      , case model.size of
          Small -> Html.Attributes.style "height" "66px"
          Big -> Html.Attributes.style "height" "88px"
      , Html.Attributes.style "overflow" "hidden"
      , Html.Attributes.style "text-overflow" "ellipsis"
      , Html.Attributes.style "white-space" "nowrap"
      , Html.Attributes.style "margin-bottom" <|
          if model.size == Big
          then "1.5em"
          else "1.0em"
      ] ++
      case model.onClick of
        Nothing -> []
        Just onClick -> [Html.Events.onClick onClick]
    )
    model.contents
