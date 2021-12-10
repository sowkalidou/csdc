module UI.BoxThread exposing
  ( view
  )

import UI.BoxItem as BoxItem
import Types exposing (Id, Thread, ThreadInfo, viewPosix)

import Html exposing (Html)
import Html.Attributes

view : Bool -> ThreadInfo -> Html (Id Thread)
view selected thread =
  let
    contents =
      [ Html.div
          [ Html.Attributes.class "is-flex is-justify-content-space-between"
          ]
          [ Html.strong [] [ Html.text thread.subject ]
          , Html.text <| viewPosix thread.createdAt
          ]
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
