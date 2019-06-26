module Page.Home exposing (..)

import Html exposing (Html, a, div, h1, li, p, text)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Decode


type alias Model =
  { allTourneysLink: Maybe String
  }

type Msg
  = Hi
  | GotLinks (Result Http.Error Links)



init: (Model, Cmd Msg)
init =
  ( Model Nothing
  , Http.get
      { url = "https://tourney-service.herokuapp.com/tourney"
      , expect = Http.expectJson GotLinks linksDecoder
      }
   )


type alias Links = { allTourneys: Maybe String }
linksDecoder: Decode.Decoder Links
linksDecoder =
  Decode.map Links (Decode.maybe (Decode.field "links" (Decode.field "allTourneysLink" Decode.string)))


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotLinks result->
      case result of
        Ok links -> ({model | allTourneysLink = links.allTourneys}, Cmd.none)
        Err _ -> (model, Cmd.none)
    Hi -> (model, Cmd.none)


view: Model -> Html a
view model =
  let
    link = case model.allTourneysLink of
      Nothing -> "no link yet"
      Just l -> l
  in
    div []
      [ h1 [] [ text "Home Page" ]
      , viewLink "#/bracket"
      , viewLink "#/create"
      , p [] [ text link ]
      ]


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
