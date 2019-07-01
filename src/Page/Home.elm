module Page.Home exposing (..)

import Html exposing (Html, a, br, div, h1, h3, input, label, li, p, span, text)
import Html.Attributes exposing (class, href, placeholder, value)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode
import List exposing (map)


-- INIT

type alias Model =
  { allTourneysLink: Maybe String
  , code: String
  , name: String
  }


init: (Model, Cmd Msg)
init =
  ( Model Nothing "" ""
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
  | Code String
  | Name String


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotLinks result->
      case result of
        Ok links -> ({model | allTourneysLink = Just links.allTourneys}, Cmd.none)
        Err _ -> (model, Cmd.none)

    Code code ->
        ({model | code = String.toUpper code}, Cmd.none)

    Name name ->
        ({model | name = String.toUpper name}, Cmd.none)


-- VIEW

view: Model -> Html Msg
view model =
  case model.allTourneysLink of
    Nothing -> div [] [ text "loading..." ]
    Just getLink ->
      div
        [ class "home-page" ]
        [ h1 [] [ text "Home Page" ]
        , div []
          [ label [] [ text "TOURNAMENT CODE" ]
          , br [] []
          , input [ value model.code, onInput Code, placeholder "ENTER 4-LETTER CODE" ] []
          ]
        , div []
          [ label [] [ text "NAME" ]
          , br [] []
          , input [ value model.name, onInput Name, placeholder "ENTER YOUR NAME"] []
          ]
        , p [ class "manage-link" ] [ a [ href ("#/manage?get-link=" ++ getLink) ] [ text "Manage Tournaments" ] ]
        ]
