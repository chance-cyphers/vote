module Page.Vote exposing (..)


import Html exposing (Html, div, h2, input, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Time



type alias Model =
  { code: String
  , match: MatchStatus
  , name: String
  , selectedIndex: Int
  }

type alias Match =
  { character1: Character
  , character2: Character
  }

type alias Character =
  { name: String
  , voteLink: String
  }

type MatchStatus
 = Loading
 | Success Match
 | Expired


-- INIT

init: String -> String -> (Model, Cmd Msg)
init code name =
  let _ = Debug.log "name" name
  in
  ( { code = code
    , match = Loading
    , name = name
    , selectedIndex = -1
    }
  , fetchMatchCmd code)


fetchMatchCmd: String -> Cmd Msg
fetchMatchCmd code =
  Http.get
    { url = ("https://tourney-service.herokuapp.com/tourney/tourney/current-match?code=" ++ code)
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
  | Tick Time.Posix


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotMatch result ->
      case result of
        Ok match ->
          ({model | match = Success match}, Cmd.none)
        Err e ->
          case e of
            Http.BadStatus status ->
              if status == 404 then ({model | match = Expired}, Cmd.none)
              else
                let _ = Debug.log "error"
                in (model, Cmd.none)
            _ ->
              let _ = Debug.log "error"
              in (model, Cmd.none)

    VoteCompleted result ->
      case result of
        Ok _ -> (model, Cmd.none)
        Err e ->
          case e of
            _ ->
              let _ = Debug.log "error" e
              in ({model | selectedIndex = -1}, Cmd.none)

    Vote1 ->
      case model.match of
        Success match -> ({model | selectedIndex = 0}, voteCmd (match.character1.voteLink ++ "?username=" ++ model.name))
        _ -> (model, Cmd.none)

    Vote2 ->
      case model.match of
        Success match -> ({model | selectedIndex = 1}, voteCmd (match.character2.voteLink ++ "?username=" ++ model.name))
        _ -> (model, Cmd.none)

    Tick _ -> (model, fetchMatchCmd model.code)


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

-- SUB

subscriptions: Model -> Sub Msg
subscriptions _ =
  Time.every 10000 Tick


-- VIEW

view: Model -> Html Msg
view model =
  case model.match of
    Loading -> div [] [ text "Loading..." ]
    Expired -> div [] [ text "Voting has ended" ]
    Success match ->
      div
        [ class "vote-page" ]
        [ div [ onClick Vote1, class (if model.selectedIndex == 0 then "vote-top selected" else "vote-top") ] [ text match.character1.name ]
        , div [ onClick Vote2, class (if model.selectedIndex == 1 then "vote-bottom selected" else "vote-bottom") ] [ text match.character2.name ]
        ]
