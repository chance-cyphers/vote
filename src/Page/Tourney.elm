module Page.Tourney exposing (..)

import Html exposing (Html, div, h1, p, text)



type alias Model =
  { tourneyLink: Maybe String
  }


init: Maybe String -> (Model, Cmd Msg)
init tourneyLink =
  ( { tourneyLink = tourneyLink
    }
  , Cmd.none
  )


type Msg = Hi

view: Model -> Html Msg
view model =
  let
    link = case model.tourneyLink of
      Just a -> a
      Nothing -> "not here yet..."
  in
    div
      []
      [ h1 [] [ text "sup from tourney page" ]
      , p [] [ text link ]
      ]
