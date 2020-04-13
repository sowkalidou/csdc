module CSDC.Input exposing
  ( button
  )

import Element exposing (Element)

import Element as Element
import Element.Font as Font
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border

button : msg -> String -> Element msg
button msg txt =
  Input.button
    [ Background.color <| Element.rgb255 142 151 164
    , Font.color <| Element.rgb255 243 243 244
    , Element.paddingXY 20 10
    ]
    { onPress = Just msg
    , label = Element.text txt
    }

