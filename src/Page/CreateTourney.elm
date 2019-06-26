module Page.CreateTourney exposing (..)

import Dict exposing (Dict, get)
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, placeholder, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import String exposing (toInt)




-- INIT

type alias Model =
    { title: String
    , matchDuration: Int
    , characters: Dict Int String
    }


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
  | Character Int String
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
      , characterInput 1 model.characters
      , characterInput 2 model.characters
      , characterInput 3 model.characters
      , characterInput 4 model.characters
      , characterInput 5 model.characters
      , characterInput 6 model.characters
      , characterInput 7 model.characters
      , characterInput 8 model.characters
      , characterInput 9 model.characters
      , characterInput 10 model.characters
      , characterInput 11 model.characters
      , characterInput 12 model.characters
      , characterInput 13 model.characters
      , characterInput 14 model.characters
      , characterInput 15 model.characters
      , characterInput 16 model.characters
      , button [ onClick Submit ] [ text "Create" ]
      ]


characterInput: Int -> Dict Int String -> Html Msg
characterInput pos characters =
  input [ type_ "text", placeholder "character", value (getCharacter pos characters), onInput (Character pos)] []


getCharacter: Int -> Dict Int String -> String
getCharacter pos dict =
  let
    maybeChar = get pos dict
  in
    case maybeChar of
      Nothing -> ""
      Just c -> c
