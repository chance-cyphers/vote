module Page.Home exposing (..)

import Html exposing (Html, a, div, h1, li, text)
import Html.Attributes exposing (href)

type Msg = Hi

view: Html a
view =
    div []
      [ h1 [] [ text "Hello from Home" ]
      , viewLink "#/bracket"
      ]


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
