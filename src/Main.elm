module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Debug exposing (toString)
import Html exposing (Html, a, div, h1, h3, li, text)
import Html.Attributes exposing (href)
import Page.Bracket exposing (Model, init)
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
    (pageModel, _) = initPageModel route
  in
    ( Model route pageModel "Hello world" navKey
    , Cmd.none
    )


initPageModel : Route -> ( PageModel, Cmd Msg )
initPageModel route =
  case route of
      Route.Home ->
        (HomeModel, Cmd.none)
      Route.Bracket ->
        let
          (bracketModel, bracketCmd) = Page.Bracket.init ()
        in
          (BracketModel bracketModel, Cmd.map BracketMsg bracketCmd)
      Route.NotFound ->
        (HomeModel, Cmd.none)



type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | BracketMsg Page.Bracket.Msg


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let _ = Debug.log "update" model
  in
  case (msg, model.pageModel) of
    (LinkClicked urlRequest, _) ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href ->
          ( model, Nav.load href )
    (UrlChanged url, _) ->
      let
        route = Route.parseRoute url
      in
        ( { model | route = route }
        , Cmd.none
        )
    (BracketMsg bMsg, BracketModel bModel) ->
      let
        (newModel, newCmd) = Page.Bracket.update bMsg bModel
      in
        ({model | pageModel = BracketModel newModel}
        , Cmd.map BracketMsg newCmd)
    (_, _) ->
      (model, Cmd.none)


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
    h3 [] [ text "Hello world" ]


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
