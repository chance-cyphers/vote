module Page.ManageTourneys exposing (..)



import Html exposing (Html, div, text)
type alias Model =
  {
  }

-- INIT

init: String -> (Model, Cmd Msg)
init _ = ({}, Cmd.none)



-- UPDATE

type Msg = Hi

update: Msg -> Model -> (Model, Cmd Msg)
update _ model = (model, Cmd.none)




-- VIEW

view: Model -> Html Msg
view _ =
  div [] [ text "blaaaaaaaaa" ]
