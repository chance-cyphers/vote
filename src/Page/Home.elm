module Page.Home exposing (..)

import Html exposing (Html, a, div, h1, h3, li, p, span, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as Decode
import List exposing (map)


-- INIT

type alias Model =
  { allTourneysLink: Maybe String
  }


init: (Model, Cmd Msg)
init =
  ( Model Nothing
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


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotLinks result->
      case result of
        Ok links -> ({model | allTourneysLink = Just links.allTourneys}, Cmd.none)
        Err _ -> (model, Cmd.none)


-- VIEW

view: Model -> Html a
view model =
  case model.allTourneysLink of
    Nothing -> div [] [ text "loading..." ]
    Just getLink ->
      div
        [ class "home-page" ]
        [ h1 [] [ text "Home Page" ]
        , p [] [ a [ href ("#/manage?get-link=" ++ getLink) ] [ text "Manage Tournaments" ] ]
        ]
