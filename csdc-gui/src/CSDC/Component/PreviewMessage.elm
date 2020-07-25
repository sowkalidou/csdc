module CSDC.Component.PreviewMessage exposing
  ( Msg (..)
  , view
  )

import CSDC.Types exposing (..)
import CSDC.Input exposing (button)

import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border

type Msg a
  = Reply
      { message : Id (Message a)
      , messageType : MessageType
      }

view : Id (Message a) -> MessageInfo a -> List (Element (Msg a))
view id (MessageInfo msg) =
  [ column
     [ height fill
     , width fill
     , spacing 10
     , Border.width 1
     , Border.color <| rgb255 92 99 118
     , Border.rounded 5
     ]
     [ row
        [ spacing 10
        , alignTop
        , Background.color <| rgb255 92 99 118
        , padding 10
        , width fill
        ]
        [ column
            [ Font.size 24 ]
            [ case msg.mtype of
                Invitation -> text <| "Invitation from " ++ msg.right
                Submission -> text <| "Submission from " ++ msg.left
            ]
        , column
            [ alignRight ]
            [ let
                event = Reply
                  { message = id
                  , messageType = msg.mtype
                  }
              in
                button event "Reply"
            ]
        ]
     , row
        [ height fill
        , padding 10
        ]
        [ text msg.text
        ]
     ]
  ]
