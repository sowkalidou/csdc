module UI.BoxImageText exposing
  ( view
  )

import UI.BoxItem as BoxItem
import Types exposing (Id, Person, FilePath, filePath)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import List

view : Bool -> id -> { value | name : String, image: FilePath } -> Html id
view selected pid value =
  let
    contents =
      [ Html.div
          [ Html.Attributes.class
            "is-flex is-flex-direction-row is-justify-content-flex-start"
          ]
          [ Html.figure
              [ Html.Attributes.class "image is-48x48"
              , Html.Attributes.style "margin" "-10px"
              , Html.Attributes.style "margin-right" "10px"
              ]
              [ Html.img
                  [ Html.Attributes.src <| filePath value.image
                  , Html.Attributes.style "border-radius" "10%"
                  , Html.Attributes.alt "Profile photo"
                  ]
                  []
              ]
          , Html.div
              [ Html.Attributes.style "width" "calc(100% - 48px)"
              , Html.Attributes.style "overflow" "hidden"
              , Html.Attributes.style "text-overflow" "ellipsis"
              ]
              [ Html.strong [] [ Html.text value.name ] ]
          ]
      ]
  in
    BoxItem.view
      { id = "box-image"
      , onClick = Just pid
      , size = BoxItem.Small
      , selected = selected
      , contents = contents
      }
