module UI.BoxFile exposing
  ( view
  )

import UI.BoxItem as BoxItem
import Types exposing (FileUI, filePath)

import Html exposing (Html)
import Html.Attributes

view : FileUI -> Html msg
view file =
  let
    contents =
      [ Html.a
          [ Html.Attributes.href <| filePath file.path
          , Html.Attributes.download file.name
          ]
          [ Html.strong [] [ Html.text file.name ]
          ]
      , Html.br [] []
      , Html.text <| "Size: " ++ String.fromInt file.size
      ]
  in
    BoxItem.view
      { id = "thread"
      , onClick = Nothing
      , size = BoxItem.Big
      , selected = False
      , contents = contents
      }
