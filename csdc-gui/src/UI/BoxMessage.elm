module UI.BoxMessage exposing
  ( view
  )

import UI.BoxItem as BoxItem
import Types exposing (Id, Message, MessageInfo, MessageType (..))

import Html exposing (Html)

view : Bool -> MessageInfo a -> Html (Id (Message a))
view selected message =
  let
    contents =
      [ Html.strong []
          [ Html.text <| case message.messageType of
              Invitation -> "Invitation from " ++ message.right
              Submission -> "Submission from " ++ message.left
          ]
      ]
  in
    BoxItem.view
      { id = "messages"
      , onClick = Just message.id
      , size = BoxItem.Small
      , selected = selected
      , contents = contents
      }
