module Page.CreateTourney exposing (..)

import Dict exposing (Dict, get)
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, placeholder, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import List exposing (map)
import String exposing (toInt)
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
  | MatchDuration String
  | Character String String
  | Submit
  | CreatedTourney (Result Http.Error String)


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
--  let _ = Debug.log "update msg" msg
--      _ = Debug.log "update model" model
--  in
  case msg of
      Hi -> (model, Cmd.none)
      Title title -> ({model | title = title}, Cmd.none)
      MatchDuration duration ->
        let
          maybeDuration = toInt duration
          parsedDuration = case maybeDuration of
            Nothing -> 0
            Just x -> x
        in
          ({model | matchDuration = parsedDuration}, Cmd.none)
      Character pos name -> ({model | characters = Dict.insert pos name model.characters}, Cmd.none)
      Submit ->
        let
          body = tourneyEncoder model
          _ = Debug.log "submit" body
        in
        ( model
        , Http.post
          { url = "https://tourney-service.herokuapp.com/tourney/tourney"
          , body = Http.jsonBody <| tourneyEncoder model
          , expect = Http.expectString CreatedTourney
          }
        )
      CreatedTourney _ ->
        (model, Cmd.none)


tourneyEncoder: Model -> Encode.Value
tourneyEncoder model =
  Encode.object
    [ ("title", Encode.string model.title)
    , ("match_duration", Encode.int model.matchDuration)
    , ("characters", Dict.values model.characters
        |> Encode.list characterEncoder
      )
    ]

characterEncoder: String -> Encode.Value
characterEncoder name =
    Encode.object [ ("name", Encode.string name) ]


-- VIEW

view: Model -> Html Msg
view model =
    div [ class "create-tourney" ]
      [ h1 [] [ text "Create page" ]
      , input [ type_ "text", placeholder "Title", value model.title, onInput Title] []
      , input [ type_ "number", step "1", placeholder "Match Duration", value (Debug.toString model.matchDuration), onInput MatchDuration] []
      , characterInput "c1" model.characters
      , characterInput "c2" model.characters
      , characterInput "c3" model.characters
      , characterInput "c4" model.characters
      , characterInput "c5" model.characters
      , characterInput "c6" model.characters
      , characterInput "c7" model.characters
      , characterInput "c8" model.characters
      , characterInput "c9" model.characters
      , characterInput "c10" model.characters
      , characterInput "c11" model.characters
      , characterInput "c12" model.characters
      , characterInput "c13" model.characters
      , characterInput "c14" model.characters
      , characterInput "c15" model.characters
      , characterInput "c16" model.characters
      , button [ onClick Submit ] [ text "Create" ]
      ]


characterInput pos characters =
  input [ type_ "text", placeholder "character", value (getCharacter pos characters), onInput (Character pos)] []


getCharacter: String -> Dict String String -> String
getCharacter pos dict =
  let
    maybeChar = get pos dict
  in
    case maybeChar of
      Nothing -> ""
      Just c -> c
