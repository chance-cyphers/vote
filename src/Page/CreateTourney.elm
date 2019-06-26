module Page.CreateTourney exposing (..)



import Html exposing (Html, div, h1, text)
type alias Model =
    {
    }


-- INIT

init: (Model, Cmd Msg)
init =
    ({}, Cmd.none)



-- UPDATE

type Msg = Hi

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Hi -> (model, Cmd.none)



-- VIEW

view: Model -> Html Msg
view model =
    div []
      [ h1 [] [ text "Create page" ]
      ]
