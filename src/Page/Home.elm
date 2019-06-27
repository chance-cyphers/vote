module Page.Home exposing (..)

import Html exposing (Html, a, div, h1, h3, li, p, text)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Decode
import List exposing (map)


-- INIT

type alias Model =
  { allTourneysLink: Maybe String
  , tourneys: List Tourney
  }

type alias Tourney =
  { title: String
  , selfLink: String
  }

init: (Model, Cmd Msg)
init =
  ( Model Nothing []
  , Http.get
      { url = "https://tourney-service.herokuapp.com/tourney"
      , expect = Http.expectJson GotLinks linksDecoder
      }
   )


type alias Links = { allTourneys: String }

linksDecoder: Decode.Decoder Links
linksDecoder =
  Decode.map Links (Decode.field "links" (Decode.field "allTourneysLink" Decode.string))



-- UPDATE

type Msg
  = GotLinks (Result Http.Error Links)
  | GotTourneys (Result Http.Error (List Tourney))


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotLinks result->
      case result of
        Ok links -> ({model | allTourneysLink = Just links.allTourneys}, fetchTourneysCmd links.allTourneys)
        Err _ -> (model, Cmd.none)
    GotTourneys result ->
      case result of
        Ok tourneys -> ({model | tourneys = tourneys}, Cmd.none)
        Err _ -> (model, Cmd.none)


fetchTourneysCmd: String -> Cmd Msg
fetchTourneysCmd link =
  Http.get
    { url = link
    , expect = Http.expectJson GotTourneys tourneysDecoder
    }

tourneysDecoder: Decode.Decoder (List Tourney)
tourneysDecoder =
  Decode.list tourneyDecoder


tourneyDecoder: Decode.Decoder Tourney
tourneyDecoder =
  Decode.map2 Tourney
    (Decode.field "title" Decode.string)
    (Decode.field "links" (Decode.field "self" Decode.string))



-- VIEW

view: Model -> Html a
view model =
  div []
    [ h1 [] [ text "Home Page" ]
    , p [] [ a [ href "#/bracket?link=https://tourney-service.herokuapp.com/tourney/bracket/example" ] [ text "Example Bracket" ] ]
    , p [] [ a [ href "#/create" ] [ text "Create Tournament" ] ]
    , h3 [] [ text "Tourneys" ]
    , div [] <| map tourneyLink model.tourneys
    ]

tourneyLink: Tourney -> Html msg
tourneyLink tourney =
  li
    []
    [ a [ href ("#/tourney/?link=" ++ tourney.selfLink)] [ text tourney.title ] ]

