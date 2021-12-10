module UI.BoxPost exposing
  ( view
  )

import Types exposing (PostInfo, filePath, viewPosix)

import Html exposing (Html)
import Html.Attributes
import Markdown

view : PostInfo -> Html msg
view post =
  Html.div
    [ Html.Attributes.class "box option-box has-background-white-ter is-shadowless"
    , Html.Attributes.style "margin-bottom" "1.0em"
    ]
    [ Html.div
        [ Html.Attributes.class
          "is-flex is-flex-direction-row is-justify-content-flex-start"
        ]
        [ Html.figure
            [ Html.Attributes.class "image is-48x48"
            , Html.Attributes.style "margin" "0px 10px 10px 0px"
            ]
            [ Html.img
                [ Html.Attributes.src <| filePath post.authorImage
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
            [ Html.strong [] [ Html.text post.authorName ]
            , Html.br [] []
            , Html.text <| viewPosix post.createdAt
            ]
        ]
    , Markdown.toHtml [] post.text
    ]
