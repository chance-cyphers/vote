module Page.Home exposing (..)

import Html exposing (Html, a, br, button, div, h1, h3, input, label, li, p, span, text)
import Html.Attributes exposing (class, href, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode


-- INIT

type alias Model =
  { allTourneysLink: Maybe String
  , code: String
  , name: String
  , errorText: String
  }


init: (Model, Cmd Msg)
init =
  ( Model Nothing "" "" ""
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
  | Submit


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

    Submit ->
      if String.isEmpty model.name then ({model | errorText = "You must enter a name"}, Cmd.none)
      else (model, Cmd.none)

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
        , button [ onClick Submit ] [ text "VOTE" ]
        , p [ class "error-text" ] [ text model.errorText ]
        , p [ class "manage-link" ] [ a [ href ("#/manage?get-link=" ++ getLink) ] [ text "Manage Tournaments" ] ]
        ]
