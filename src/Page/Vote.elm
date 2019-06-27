module Page.Vote exposing (..)


import Html exposing (Html, div, h1, p, text)
import Http
import Json.Decode as Decode



type alias Model =
  { currentMatchLink: String
  , match: Maybe Match
  }

type alias Match =
  { character1: Character
  , character2: Character
  }

type alias Character =
  { name: String
  }


-- INIT

init: String -> (Model, Cmd Msg)
init currentMatchLink = (Model currentMatchLink Nothing, fetchMatchCmd currentMatchLink)


fetchMatchCmd: String -> Cmd Msg
fetchMatchCmd link =
  Http.get
    { url = link
    , expect = Http.expectJson GotMatch matchDecoder
    }


matchDecoder: Decode.Decoder Match
matchDecoder =
  Decode.map2 Match
    (Decode.field "character1" characterDecoder)
    (Decode.field "character2" characterDecoder)


characterDecoder: Decode.Decoder Character
characterDecoder =
  Decode.map Character (Decode.field "name" Decode.string)


-- UPDATE

type Msg = GotMatch (Result Http.Error Match)


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotMatch result ->
      case result of
        Ok match ->
          ({model | match = Just match}, Cmd.none)
        Err e ->
          let _ = Debug.log "error" e
          in (model, Cmd.none)


-- VIEW

view: Model -> Html Msg
view model =
  case model.match of
    Nothing -> div [] [ text "loading..." ]
    Just match ->
      div
        []
        [ h1 [] [ text "Vote Page"]
        , p [] [ text match.character1.name ]
        , p [] [ text "VS" ]
        , p [] [ text match.character2.name ]
        ]
