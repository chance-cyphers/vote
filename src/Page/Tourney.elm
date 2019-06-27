module Page.Tourney exposing (..)

import Html exposing (Html, div, h1, p, text)



type alias Model =
  { tourneyLink: String
  }


init: String -> (Model, Cmd Msg)
init tourneyLink =
  ( { tourneyLink = tourneyLink
    }
  , Cmd.none
  )


type Msg = Hi






view: Model -> Html Msg
view model =
    div
      []
      [ h1 [] [ text "sup from tourney page" ]
      , p [] [ text model.tourneyLink ]
      ]
