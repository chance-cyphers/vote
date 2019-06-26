module Page.Home exposing (..)

import Html exposing (Html, a, div, h1, li, p, text)
import Html.Attributes exposing (href)


type alias Model =
  { allTourneysLink: Maybe String
  }

type Msg = Hi


init: Maybe String -> (Model, Cmd Msg)
init allTourneysLink =
  ( Model allTourneysLink
  , Cmd.none
  )


view: Model -> Html a
view model =
  let
    link = case model.allTourneysLink of
      Nothing -> "no link yet"
      Just l -> l
  in
    div []
      [ h1 [] [ text "Home Page" ]
      , viewLink "#/bracket"
      , viewLink "#/create"
      , p [] [ text link ]
      ]


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
