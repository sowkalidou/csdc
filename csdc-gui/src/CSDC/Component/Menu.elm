module CSDC.Component.Menu exposing
  ( Model (..)
  , initial
  , toPage
  , fromPage
  , Msg (..)
  , view
  )

import CSDC.Types exposing (..)
import CSDC.API as API
import CSDC.Page as Page

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Json
import Maybe
import Maybe exposing (withDefault)
import String

--------------------------------------------------------------------------------
-- Model

type Model
  = Explorer
  | Studio
  | Admin

initial : Model
initial = Studio

toPage : Model -> Page.Page
toPage model =
  case model of
    Explorer -> Page.Explorer
    Studio -> Page.Studio
    Admin -> Page.Admin

fromPage : Page.Page -> Maybe Model
fromPage page =
  case page of
    Page.Explorer -> Just Explorer
    Page.Studio -> Just Studio
    Page.Admin -> Just Admin
    _ -> Nothing

--------------------------------------------------------------------------------
-- Update

type Msg = SetItem Model

--------------------------------------------------------------------------------
-- View

-- the target makes the link not redirect to /
item : Maybe Model -> String -> Model -> Html Msg
item model name this =
  Html.a
    ( if model == Just this
      then
        [ Html.Events.onClick (SetItem this)
        , Html.Attributes.class "is-active"
        , Html.Attributes.target "_self"
        ]
      else
        [ Html.Events.onClick (SetItem this)
        , Html.Attributes.target "_self"
        ]
    )
    [ Html.text name
    ]

title : String -> Html msg
title txt =
  Html.p
    [ Html.Attributes.class "menu-label"
    ]
    [ Html.text txt
    ]

list : List (Html msg) -> Html msg
list items =
  Html.ul
    [ Html.Attributes.class "menu-list"
    ]
    (List.map (\a -> Html.li [] [a]) items)

view : Maybe Model -> Html Msg
view model =
  Html.div
    [ Html.Attributes.class "menu"
    ]
    [ title "General"
    , list
        [ item model "Studio" Studio
        , item model "Explorer" Explorer
        ]
    , title "Administration"
    , list
        [ item model "Admin" Admin
        ]
    ]

