module Page.Vote exposing (..)


import Html exposing (Html, div, h1, text)



type alias Model =
  { currentMatchLink: String
  }


init: String -> (Model, Cmd Msg)
init currentMatchLink = (Model currentMatchLink, Cmd.none)


type Msg = Hi


update: Msg -> Model -> (Model, Cmd Msg)
update _ model = (model, Cmd.none)


view: Model -> Html Msg
view model =
  div
    []
    [ h1 [] [ text "Vote Page"]
    ]
