module CSDC.UI.BoxThread exposing
  ( view
  )

import CSDC.UI.BoxItem as BoxItem
import CSDC.Types exposing (Id, Thread, ThreadInfo)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import List

view : Bool -> ThreadInfo -> Html (Id Thread)
view selected thread =
  let
    contents =
      [ Html.strong [] [ Html.text thread.subject ]
      , Html.br [] []
      , Html.div
          [ Html.Attributes.class "is-flex is-justify-content-space-between"
          ]
          [ Html.em [] [ Html.text <| "Author: " ++ thread.authorName ]
          , Html.text <| String.fromInt thread.messages ++ " posts"
          ]
      ]
  in
    BoxItem.view
      { id = "thread"
      , onClick = Just thread.id
      , size = BoxItem.Big
      , selected = selected
      , contents = contents
      }
