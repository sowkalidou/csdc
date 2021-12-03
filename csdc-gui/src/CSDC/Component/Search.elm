module CSDC.Component.Search exposing
  ( Model
  , initial
  , Msg
  , update
  , view
  )

import CSDC.API as API
import CSDC.Page as Page
import CSDC.Types exposing (..)

import FeatherIcons
import Html exposing (Html)
import Html.Attributes
import Html.Events

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { input: String
  , results: List (SearchResult SearchId)
  , loading: Bool
  }

initial : Model
initial =
  { input = ""
  , results = []
  , loading = False
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = Input String
  | Click SearchId
  | Search (API.Response (List (SearchResult SearchId)))

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  case msg of
    Input val ->
      if String.length val < 3
      then
        ( { model | input = val, results = [], loading = False }
        , Cmd.none
        )
      else
        ( { model | input = val, loading = True }
        , Cmd.map Search <| API.searchAll val
        )

    Click val ->
      ( initial
      , Page.goTo pageInfo <|
          case val of
            SearchUnit id -> Page.Unit id
            SearchPerson id -> Page.Person id
      )

    Search result ->
      case result of
        Err _ ->
          ( { model | loading = False }
          , Cmd.none
          )
        Ok results ->
          ( { model | results = results , loading = False }
          , Cmd.none
          )

--------------------------------------------------------------------------------
-- View

view : Model -> Html Msg
view model =
  Html.div
    [ if List.isEmpty model.results
      then Html.Attributes.class "dropdown is-right"
      else Html.Attributes.class "dropdown is-active is-right"
    , Html.Attributes.style "height" "26px"
    , Html.Attributes.style "width" "300px"
    , Html.Attributes.style "margin-top" "-4px"
    ]
    [ Html.div
        [ Html.Attributes.class "dropdown-trigger"
        , Html.Attributes.style "width" "100%"
        ]
        [ Html.p
            [ if model.loading
              then Html.Attributes.class "control has-icons-left is-loading"
              else Html.Attributes.class "control has-icons-left"
            , Html.Attributes.style "margin-top" "-5px"
            ]
            [ Html.input
                [ Html.Attributes.class "input"
                , Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "Search"
                , Html.Attributes.value model.input
                , Html.Events.onInput Input
                , Html.Attributes.style "width" "100%"
                ]
                []
            , Html.span
                [ Html.Attributes.class "icon is-small is-left" ]
                [ FeatherIcons.toHtml [] FeatherIcons.search ]
            ]
        ]
    , Html.div
        [ Html.Attributes.class "dropdown-menu"
        , Html.Attributes.attribute "role" "menu"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "margin-top" "10px"
        ]
        [ Html.div
            [ Html.Attributes.class "dropdown-content"
            ]
            (List.map viewSearchResult model.results)
        ]
    ]

viewSearchResult : SearchResult SearchId -> Html Msg
viewSearchResult { name, id } =
  Html.div
    [ Html.Attributes.class "dropdown-item"
    , Html.Events.onClick (Click id)
    ]
    [ Html.text name
    ]
