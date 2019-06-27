module Page.Tourney exposing (..)

import Html exposing (Html, a, div, h1, h3, p, text)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Decode



type alias Model =
  { tourneyLink: String
  , tourney: Maybe Tourney
  }

type alias Tourney =
  { title: String
  , matchDuration: Int
  , characters: List Character
  , currentMatchLink: String
  }

type alias Character =
  { name: String
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
  Decode.map4 Tourney
    (Decode.field "title" Decode.string)
    (Decode.field "match_duration" Decode.int)
    (Decode.field "characters" (Decode.list characterDecoder))
    (Decode.field "links" (Decode.field "currentMatch" Decode.string))


characterDecoder: Decode.Decoder Character
characterDecoder =
  Decode.map Character (Decode.field "name" Decode.string)


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
        [ h1 [] [ text tourney.title ]
        , div [] (List.map (\n -> p [] [text n.name]) tourney.characters)
        , a [ href ("#/vote?link=" ++ tourney.currentMatchLink) ] [ text "Vote" ]
        ]

