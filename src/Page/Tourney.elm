module Page.Tourney exposing (..)

import Html exposing (Html, div, h1, p, text)
import Http
import Json.Decode as Decode



type alias Model =
  { tourneyLink: String
  , tourney: Maybe Tourney
  }

type alias Tourney =
  { title: String
  , matchDuration: Int
  }


init: String -> (Model, Cmd Msg)
init tourneyLink =
  ( { tourneyLink = tourneyLink
    , tourney = Nothing
    }
  , fetchTourneyCmd tourneyLink
  )


fetchTourneyCmd: String -> Cmd Msg
fetchTourneyCmd link =
  Http.get
    { url = link
    , expect = Http.expectJson GotTourney tourneyDecoder
    }

tourneyDecoder: Decode.Decoder Tourney
tourneyDecoder =
  Decode.map2 Tourney
    (Decode.field "title" Decode.string)
    (Decode.field "match_duration" Decode.int)


type Msg
  = GotTourney (Result Http.Error Tourney)



update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotTourney result ->
      case result of
        Ok tourney -> ({model | tourney = Just tourney}, Cmd.none)
        Err e ->
          let _ = Debug.log "error fetching tourney" e
          in (model, Cmd.none)


view: Model -> Html Msg
view model =
  case model.tourney of
    Nothing -> div [] [ text "loading..." ]
    Just tourney ->
      div
        []
        [ h1 [] [ text "sup from tourney page" ]
        , p [] [ text model.tourneyLink ]
        , p [] [ text tourney.title ]
        ]

