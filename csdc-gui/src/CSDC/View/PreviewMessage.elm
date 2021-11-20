module CSDC.View.PreviewMessage exposing
  ( Msg (..)
  , view
  )

import CSDC.Component.Preview as Preview
import CSDC.Types exposing (..)
import CSDC.Input exposing (button)

import Html exposing (Html)
import Html.Attributes
import Html.Events

type Msg a
  = Reply
      { message : Id (Message a)
      , messageType : MessageType
      }

-- XXX: Put more information
view : Id (Message a) -> MessageInfo a -> Html (Msg a)
view id (MessageInfo msg) = Preview.make
  [ Html.p
      []
      [ Html.strong []
          [ Html.text <|
            case msg.mtype of
              Invitation -> "Invitation from " ++ msg.right
              Submission -> "Submission from " ++ msg.left
          ]
      , Html.br [] []
      , Html.text msg.text
      ]
  , Html.button
      [ Html.Attributes.class "button is-primary is-pulled-right"
      , Html.Events.onClick <| Reply { message = id, messageType = msg.mtype }
      ]
      [ Html.text "Reply"
      ]
  ]
