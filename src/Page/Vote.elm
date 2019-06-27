module Page.Vote exposing (..)


import Html exposing (Html, button, div, h1, p, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode



type alias Model =
  { currentMatchLink: String
  , match: Maybe Match
  }

type alias Match =
  { character1: Character
  , character2: Character
  }

type alias Character =
  { name: String
  , voteLink: String
  }


-- INIT

init: String -> (Model, Cmd Msg)
init currentMatchLink = (Model currentMatchLink Nothing, fetchMatchCmd currentMatchLink)


fetchMatchCmd: String -> Cmd Msg
fetchMatchCmd link =
  Http.get
    { url = link
    , expect = Http.expectJson GotMatch matchDecoder
    }


matchDecoder: Decode.Decoder Match
matchDecoder =
  Decode.map2 Match
    (Decode.field "character1" characterDecoder)
    (Decode.field "character2" characterDecoder)


characterDecoder: Decode.Decoder Character
characterDecoder =
  Decode.map2 Character
    (Decode.field "name" Decode.string)
    (Decode.field "voteLink" Decode.string)


-- UPDATE

type Msg
  = GotMatch (Result Http.Error Match)
  | VoteCompleted (Result Http.Error String)
  | Vote1
  | Vote2


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotMatch result ->
      case result of
        Ok match ->
          ({model | match = Just match}, Cmd.none)
        Err e ->
          let _ = Debug.log "error" e
          in (model, Cmd.none)

    VoteCompleted result ->
      case result of
        Ok _ -> (model, Cmd.none)
        Err e ->
          let _ = Debug.log "error" e
          in (model, Cmd.none)

    Vote1 ->
      case model.match of
        Nothing -> (model, Cmd.none)
        Just match -> (model, voteCmd (match.character1.voteLink ++ "?username=bob"))

    Vote2 ->
      case model.match of
        Nothing -> (model, Cmd.none)
        Just match -> (model, voteCmd (match.character2.voteLink ++ "?username=bob"))


voteCmd: String -> Cmd Msg
voteCmd link =
  Http.request
    { method = "PUT"
    , headers = []
    , url = link
    , body = Http.emptyBody
    , expect = Http.expectString VoteCompleted
    , timeout = Nothing
    , tracker = Nothing
    }

-- VIEW

view: Model -> Html Msg
view model =
  case model.match of
    Nothing -> div [] [ text "loading..." ]
    Just match ->
      div
        []
        [ h1 [] [ text "Vote Page"]
        , button [ onClick Vote1 ] [ text match.character1.name]
        , p [] [ text "VS" ]
        , button [ onClick Vote2 ] [ text match.character2.name]
        ]