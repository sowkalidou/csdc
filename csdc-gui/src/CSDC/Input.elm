module CSDC.Input exposing
  ( button
  , buttonDanger
  , text
  , textarea
  )

import Field exposing (Field (..), Status (..))

import Html exposing (Html)
import Html.Attributes
import Html.Events

--------------------------------------------------------------------------------
-- Button

button : msg -> String -> Html msg
button msg txt =
  Html.button
    [ Html.Attributes.class "button is-link is-pulled-right"
    , Html.Events.onClick msg
    ]
    [ Html.text txt
    ]

buttonDanger : msg -> String -> Html msg
buttonDanger msg txt =
  Html.button
    [ Html.Attributes.class "button is-danger is-pulled-right"
    , Html.Events.onClick msg
    ]
    [ Html.text txt
    ]

--------------------------------------------------------------------------------
-- Text input

wrapper : Field a b -> Html msg -> Html msg
wrapper field div =
  Html.div
    [ Html.Attributes.class "field"
    ] <|
    [ Html.label
        [ Html.Attributes.class "label" ]
        [ Html.text (Field.name field) ]
    , Html.div
        [ Html.Attributes.class "control" ]
        [ div ]
    ] ++
      let
        makeError err =
          Html.p
            [ Html.Attributes.class "help is-danger" ]
            [ Html.text err ]
      in
        List.map makeError (Field.errors field)

text : Field String a -> (String -> msg) -> Html msg
text field makeMsg =
  wrapper field <|
    Html.input
      [ case Field.status field of
          Invalid _ -> Html.Attributes.class "input is-danger"
          _ -> Html.Attributes.class "input"
      , Html.Attributes.type_ "text"
      , Html.Attributes.placeholder (Field.name field)
      , Html.Attributes.value (Field.raw field)
      , Html.Events.onInput makeMsg
      ]
      []


textarea : Field String a -> (String -> msg) -> Html msg
textarea field makeMsg =
  wrapper field <|
    Html.textarea
      [ case Field.status field of
          Invalid _ -> Html.Attributes.class "textarea is-danger"
          _ -> Html.Attributes.class "textarea"
      , Html.Attributes.placeholder (Field.name field)
      , Html.Attributes.value (Field.raw field)
      , Html.Events.onInput makeMsg
      ]
      []

--------------------------------------------------------------------------------
-- Text input

dropdown : List (a, String) -> Field (Maybe a) b -> (Maybe a -> msg) -> Html msg
dropdown items field makeMsg =
  wrapper field <|
    Html.div
      [ Html.Attributes.class "dropdown"
      , Html.Attributes.style "height" "26px"
      , Html.Attributes.style "margin-top" "-4px"
      ]
      [ Html.div
          [ Html.Attributes.class "dropdown-trigger"
          ]
          [ Html.span [] [ Html.text "Select" ]
          , Html.span [] [ Html.text "\u{65088}" ]
          ]
      , Html.div
          [ Html.Attributes.class "dropdown-menu"
          , Html.Attributes.attribute "role" "menu"
          ]
          [ Html.div
              [ Html.Attributes.class "dropdown-content has-text-right"
              ]
              (List.map (Html.map makeMsg << dropdownItem) items)
          ]
      ]

dropdownItem : (a, String) -> Html (Maybe a)
dropdownItem (a, label) =
  Html.div
    [ Html.Attributes.class "dropdown-item"
    , Html.Events.onClick (Just a)
    ]
    [ Html.text label
    ]
