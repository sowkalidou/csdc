module CSDC.UI.Navbar exposing
  ( view
  )

import Html exposing (Html)
import Html.Attributes

view : Html msg -> Html msg
view search =
  Html.nav
    [ Html.Attributes.class "navbar is-fixed-top"
    , Html.Attributes.attribute "role" "navigation"
    , Html.Attributes.attribute "aria-label" "main navigation"
    ]
    [ Html.div
        [ Html.Attributes.class "navbar-brand"
        ]
        [ Html.a
            [ Html.Attributes.class "navbar-item"
            , Html.Attributes.href "https://www.cs-dc.org/"
            , Html.Attributes.target "_blank"
            ]
            [ Html.h1
                [ Html.Attributes.class "title" ]
                [ Html.strong [] [ Html.text "CS-DC DAO" ] ]
            ]
        , Html.a
            [ Html.Attributes.class "navbar-burger"
            , Html.Attributes.attribute "role" "button"
            , Html.Attributes.attribute "aria-label" "menu"
            , Html.Attributes.attribute "aria-expanded" "false"
            , Html.Attributes.attribute "data-target" "navbar-basic"
            ]
            [ Html.span [ Html.Attributes.attribute "aria-hidden" "true" ] []
            , Html.span [ Html.Attributes.attribute "aria-hidden" "true" ] []
            , Html.span [ Html.Attributes.attribute "aria-hidden" "true" ] []
            ]
        ]
    , Html.div
        [ Html.Attributes.id "navbar-basic"
        , Html.Attributes.class "navbar-menu"
        ]
        [ Html.div
            [ Html.Attributes.class "navbar-start"
            ]
            [ Html.a
                [ Html.Attributes.class "navbar-item"
                , Html.Attributes.href "https://www.cs-dc.org/"
                , Html.Attributes.target "_blank"
                ]
                [ Html.text "Complex Systems Digital Campus"
                ]
            ]
        , Html.div
            [ Html.Attributes.class "navbar-end"
            ]
            [ Html.div
                [ Html.Attributes.class "navbar-item" ]
                [ search ]
            , Html.div
                [ Html.Attributes.class "navbar-item" ]
                [ Html.div
                    [ Html.Attributes.class "buttons"
                    ]
                    [ Html.a
                        [ Html.Attributes.class "button is-light" ]
                        [ Html.text "Log Out" ]
                    ]
                ]
            ]
        ]
    ]
