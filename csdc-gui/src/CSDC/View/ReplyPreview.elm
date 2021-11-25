module CSDC.View.ReplyPreview exposing
  ( Msg (..)
  , view
  )

import CSDC.Component.Preview as Preview
import CSDC.Types exposing (..)
import CSDC.Input exposing (button)
import CSDC.View.MessagePreview exposing (viewMessage)

import Html exposing (Html)
import Html.Attributes
import Html.Events

type Msg a = MarkAsSeen (Id (Reply a))

view : Id (Reply a) -> ReplyInfo a -> Html (Msg a)
view id msg = Preview.make <|
  [ Html.h4 []
      [ Html.text <|
        ( case msg.mtype of
            Invitation -> "Invitation"
            Submission -> "Submission"
        ) ++ " " ++
        ( case msg.rtype of
            Accept -> "Accepted"
            Reject -> "Rejected"
        )
      ]
  ] ++
  [ Html.p
      []
      [ Html.text msg.text
      , Html.hr [] []
      ]
  , Html.h4 [] [ Html.text "Original Message" ]
  ] ++
  viewMessage msg.message ++
  [ Html.hr [] []
  , Html.button
      [ Html.Attributes.class "button is-primary is-pulled-right"
      , Html.Events.onClick (MarkAsSeen id)
      ]
      [ Html.text "Mark as seen"
      ]
  ]
