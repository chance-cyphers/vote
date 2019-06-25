module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Debug exposing (log, toString)
import Html exposing (Html, a, div, h1, li, p, text)
import Html.Attributes exposing (href)
import Page.Bracket exposing (Model)
import Url
import Route exposing (Route)


main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



type alias Model =
  { route: Route
  , pageModel: PageModel
  , greeting: String
  , key: Nav.Key
  }


type PageModel
  = HomeModel
  | BracketModel Page.Bracket.Model


-- INIT


init: () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url navKey =
  let
    route = Route.parseRoute url
  in
    ( Model route HomeModel "Hello world" navKey
    , Cmd.none
    )


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href ->
          ( model, Nav.load href )
    UrlChanged url ->
      let
        route = Route.parseRoute url
        s = log "asdasdsdasd" (toString Url.Http)
      in
        ( { model | route = route }
        , Cmd.none
        )



subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.none



view: Model -> Document Msg
view model =
  { title = "Hi"
  , body =
      [ h1 [] [ text (toString model.route)]
      , div [] (List.map renderARow [1, 2, 3])
      , viewLink "#/bracket"
      ]
  }


renderARow _ =
    h1 [] [ text "Hello world" ]


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
