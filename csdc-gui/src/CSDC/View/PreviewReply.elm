module CSDC.View.PreviewReply exposing
  ( Msg (..)
  , view
  )

import CSDC.Component.Preview as Preview
import CSDC.Types exposing (..)
import CSDC.Input exposing (button)

import Html exposing (Html)
import Html.Attributes
import Html.Events

type Msg a = MarkAsSeen (Id (Reply a))

-- XXX: Put more information
view : Id (Reply a) -> ReplyInfo a -> Html (Msg a)
view id (ReplyInfo msg) = Preview.make
  [ Html.p
      []
      [ Html.strong []
          [ Html.text <|
            case msg.rtype of
              Accept -> "Accepted"
              Reject -> "Rejected"
          ]
      , Html.br [] []
      , Html.text msg.text
      ]
  , Html.button
      [ Html.Attributes.class "button is-primary is-pulled-right"
      , Html.Events.onClick (MarkAsSeen id)
      ]
      [ Html.text "Mark as seen"
      ]
  ]
