module Page.Vote exposing (..)


import Html exposing (Html, button, div, h1, input, p, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode



type alias Model =
  { currentMatchLink: String
  , match: MatchStatus
  , username: String
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

init: String -> (Model, Cmd Msg)
init currentMatchLink = (Model currentMatchLink Loading "", fetchMatchCmd currentMatchLink)


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
  | Username String


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
              in (model, Cmd.none)


    Vote1 ->
      case model.match of
        Success match -> (model, voteCmd (match.character1.voteLink ++ "?username=" ++ model.username))
        _ -> (model, Cmd.none)

    Vote2 ->
      case model.match of
        Success match -> (model, voteCmd (match.character2.voteLink ++ "?username=" ++ model.username))
        _ -> (model, Cmd.none)

    Username name -> ({model | username = name}, Cmd.none)


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
    Loading -> div [] [ text "Loading..." ]
    Expired -> div [] [ text "Match is expired" ]
    Success match ->
      div
        [ class "vote-page" ]
        [ h1 [] [ text "Vote Page"]
        , button [ onClick Vote1 ] [ text match.character1.name]
        , input [ type_ "text", placeholder "username", value model.username, onInput Username ] [ text "VS" ]
        , button [ onClick Vote2 ] [ text match.character2.name]
        ]
