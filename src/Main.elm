module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, h1, text)
import Page.Bracket exposing (Model, init)
import Page.CreateTourney
import Page.Tourney
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
  }


type PageModel
  = HomeModel Page.Home.Model
  | BracketModel Page.Bracket.Model
  | TourneyModel Page.Tourney.Model
  | CreateTourneyModel Page.CreateTourney.Model
  | NotFoundModel


-- INIT


init: () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url navKey =
  let
    route = Route.parseRoute url
    (pageModel, pageCmd) = initPageModel route
  in (Model route pageModel navKey, pageCmd)


initPageModel : Route -> (PageModel, Cmd Msg)
initPageModel route =
  case route of
      Route.Home ->
        let (homeModel, homeCmd) = Page.Home.init
        in (HomeModel homeModel, Cmd.map HomeMsg homeCmd)
      Route.Bracket ->
        let (bracketModel, bracketCmd) = Page.Bracket.init ()
        in (BracketModel bracketModel, Cmd.map BracketMsg bracketCmd)
      Route.Tourney link ->
        let (tourneyModel, tourneyCmd) = Page.Tourney.init link
        in (TourneyModel tourneyModel, Cmd.map TourneyMsg tourneyCmd)
      Route.CreateTourney ->
        let (createModel, createCmd) = Page.CreateTourney.init
        in (CreateTourneyModel createModel, Cmd.map CreateTourneyMsg createCmd)
      Route.NotFound ->
        (NotFoundModel, Cmd.none)



type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | BracketMsg Page.Bracket.Msg
  | TourneyMsg Page.Tourney.Msg
  | CreateTourneyMsg Page.CreateTourney.Msg
  | HomeMsg Page.Home.Msg


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
      let (route, pageModel, cmd) = updateRoute url
      in ( { model | route = route, pageModel = pageModel }, cmd)

    (BracketMsg bMsg, BracketModel bModel) ->
      let (newModel, newCmd) = Page.Bracket.update bMsg bModel
      in ({model | pageModel = BracketModel newModel}, Cmd.map BracketMsg newCmd)

    (CreateTourneyMsg cMsg, CreateTourneyModel cModel) ->
      let (newModel, newCmd) = Page.CreateTourney.update cMsg cModel
      in ({model | pageModel = CreateTourneyModel newModel}, Cmd.map CreateTourneyMsg newCmd)

    (HomeMsg hMsg, HomeModel hModel) ->
      let (newModel, newCmd) = Page.Home.update hMsg hModel
      in ({ model | pageModel = HomeModel newModel}, Cmd.map HomeMsg newCmd)

    (_, _) -> (model, Cmd.none)



updateRoute: Url -> (Route, PageModel, Cmd Msg)
updateRoute url =
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
          let (homeModel, homeCmd) = Page.Home.init
          in (Home, HomeModel homeModel, Cmd.map HomeMsg homeCmd)

        Tourney maybeLink ->
          let (tourneyModel, tourneyCmd) = Page.Tourney.init maybeLink
          in (Tourney maybeLink, TourneyModel tourneyModel, Cmd.map TourneyMsg tourneyCmd)

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
    HomeModel homeModel -> Page.Home.view homeModel
    BracketModel bracketModel -> Html.map BracketMsg <| Page.Bracket.view bracketModel
    TourneyModel tourneyModel -> Html.map TourneyMsg <| Page.Tourney.view tourneyModel
    CreateTourneyModel createModel -> Html.map CreateTourneyMsg <| Page.CreateTourney.view createModel
    NotFoundModel -> h1 [] [ text "404 Page Not Found" ]
