module Page.ManageTourneys exposing (..)



import Html exposing (Html, a, div, h1, h3, li, p, span, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as Decode
import List exposing (map)
type alias Model =
  { allTourneysLink: String
  , tourneys: List Tourney
  }

type alias Tourney =
  { title: String
  , selfLink: String
  , bracketLink: String
  , matchLink: String
  }


-- INIT

init: String -> (Model, Cmd Msg)
init allTourneysLink = (Model allTourneysLink [], fetchTourneysCmd allTourneysLink)



-- UPDATE

type Msg = GotTourneys (Result Http.Error (List Tourney))

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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
  Decode.map4 Tourney
    (Decode.field "title" Decode.string)
    (Decode.field "links" (Decode.field "self" Decode.string))
    (Decode.field "links" (Decode.field "bracket" Decode.string))
    (Decode.field "links" (Decode.field "currentMatch" Decode.string))


-- VIEW

view: Model -> Html Msg
view model =
  div
    [ class "manage-tourneys-page"]
    [ h1 [] [ text "Manage Tournaments" ]
    , p [] [ a [ href "#/create" ] [ text "Create Tournament" ] ]
    , h3 [] [ text "Tournaments" ]
    , div [] <| map tourneyItem model.tourneys
    ]

tourneyItem: Tourney -> Html msg
tourneyItem tourney =
  li
    []
    [ span [] [ text tourney.title ]
    , a [ href ("#/bracket/?link=" ++ tourney.bracketLink), class "tourney-link" ]
      [ text "bracket" ]
    , a [ href ("#/vote/?link=" ++ tourney.matchLink ++ "&name=" ++ "bob"), class "tourney-link" ]
      [ text "vote" ]
    ]

