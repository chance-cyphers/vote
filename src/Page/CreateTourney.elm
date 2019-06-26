module Page.CreateTourney exposing (..)



import Dict exposing (Dict)
import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput)
type alias Model =
    { title: String
    , matchDuration: Int
    , characters: Dict String String
    }


-- INIT

init: (Model, Cmd Msg)
init =
    ({ title = ""
      , matchDuration = 0
      , characters = Dict.empty
      }
    , Cmd.none
    )



-- UPDATE

type Msg
  = Hi
  | Title String


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let _ = Debug.log "update" model
  in
  case msg of
      Hi -> (model, Cmd.none)
      Title title -> ({model | title = title}, Cmd.none)



-- VIEW

view: Model -> Html Msg
view model =
    div []
      [ h1 [] [ text "Create page" ]
      , input [ type_ "text", placeholder "Title", value model.title, onInput Title] []
      ]
