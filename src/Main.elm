module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, h1, text)
import Http
import Json.Decode as Decode
import Page.Bracket exposing (Model, init)
import Page.CreateTourney
import Page.Home
import Url exposing (Url)
import Route exposing (Route(..))


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
  , key: Nav.Key
  , links: Links
  }

type alias Links = { allTourneys: Maybe String }

type PageModel
  = HomeModel Page.Home.Model
  | BracketModel Page.Bracket.Model
  | CreateTourneyModel Page.CreateTourney.Model
  | NotFoundModel


-- INIT


init: () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url navKey =
  let
    route = Route.parseRoute url
    (pageModel, _) = initPageModel route
  in
    ( Model route pageModel navKey { allTourneys = Nothing }
    , Http.get
      { url = "https://tourney-service.herokuapp.com/tourney"
      , expect = Http.expectJson GotLinks linksDecoder}
    )

linksDecoder: Decode.Decoder Links
linksDecoder =
  Decode.map Links (Decode.maybe (Decode.field "links" (Decode.field "allTourneysLink" Decode.string)))


initPageModel : Route -> ( PageModel, Cmd Msg )
initPageModel route =
  case route of
      Route.Home ->
        let (homeModel, homeCmd) = Page.Home.init Nothing
        in (HomeModel homeModel, Cmd.map HomeMsg homeCmd)
      Route.Bracket ->
        let (bracketModel, bracketCmd) = Page.Bracket.init ()
        in (BracketModel bracketModel, Cmd.map BracketMsg bracketCmd)
      Route.CreateTourney ->
        let (createModel, createCmd) = Page.CreateTourney.init
        in (CreateTourneyModel createModel, Cmd.map CreateTourneyMsg createCmd)
      Route.NotFound ->
        (NotFoundModel, Cmd.none)



type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | BracketMsg Page.Bracket.Msg
  | CreateTourneyMsg Page.CreateTourney.Msg
  | HomeMsg Page.Home.Msg
  | GotLinks (Result Http.Error Links)


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model.pageModel) of
    (LinkClicked urlRequest, _) ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href ->
          ( model, Nav.load href )

    (UrlChanged url, _) ->
      let
        (route, pageModel, cmd) = updateRoute url model
      in
        ( { model | route = route, pageModel = pageModel }
        , cmd
        )

    (BracketMsg bMsg, BracketModel bModel) ->
      let
        (newModel, newCmd) = Page.Bracket.update bMsg bModel
      in
        ({model | pageModel = BracketModel newModel}
        , Cmd.map BracketMsg newCmd
        )

    (CreateTourneyMsg cMsg, CreateTourneyModel cModel) ->
      let
        (newModel, newCmd) = Page.CreateTourney.update cMsg cModel
      in
        ({model | pageModel = CreateTourneyModel newModel}
        , Cmd.map CreateTourneyMsg newCmd
        )
    (GotLinks linksResult, _) ->
      let _ = Debug.log "links" linksResult
      in
      case linksResult of
        Ok links ->
          case model.pageModel of
            HomeModel m -> ({model | links = links, pageModel = HomeModel {m | allTourneysLink = links.allTourneys}}, Cmd.none)
            _ -> ({model | links = links}, Cmd.none)
        Err _ -> (model, Cmd.none)

    (_, _) -> (model, Cmd.none)



updateRoute: Url -> Model -> (Route, PageModel, Cmd Msg)
updateRoute url model =
    let route = Route.parseRoute url
    in
      case route of
        Bracket ->
          let (bracketModel, bracketCmd) = Page.Bracket.init ()
          in (route, BracketModel bracketModel, Cmd.map BracketMsg bracketCmd)

        CreateTourney ->
          let (createModel, createCmd) = Page.CreateTourney.init
          in (route, CreateTourneyModel createModel, Cmd.map CreateTourneyMsg createCmd)

        Home ->
          let (homeModel, homeCmd) = Page.Home.init model.links.allTourneys
          in (Home, HomeModel homeModel, Cmd.map HomeMsg homeCmd)

        NotFound -> (NotFound, NotFoundModel, Cmd.none)



subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.none



view: Model -> Document Msg
view model =
  { title = "Hi"
  , body =
      [ pageContent model
      ]
  }

pageContent : Model -> Html Msg
pageContent model =
    case model.pageModel of
        HomeModel m -> Page.Home.view m
        BracketModel bracketModel -> Html.map BracketMsg <| Page.Bracket.view bracketModel
        CreateTourneyModel createModel -> Html.map CreateTourneyMsg <| Page.CreateTourney.view createModel
        NotFoundModel -> h1 [] [ text "404 Page Not Found" ]
