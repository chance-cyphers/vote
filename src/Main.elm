module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, h1, text)
import Page.Bracket exposing (Model, init)
import Page.CreateTourney
import Page.Home
import Page.ManageTourneys
import Page.Vote
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
  | CreateTourneyModel Page.CreateTourney.Model
  | ManageTourneysModel Page.ManageTourneys.Model
  | VoteModel Page.Vote.Model
  | NotFoundModel


-- INIT


init: () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url navKey =
  let
    (route, model, cmd) = updateRoute url navKey
  in (Model route model navKey, cmd)



type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | BracketMsg Page.Bracket.Msg
  | VoteMsg Page.Vote.Msg
  | ManageTourneysMsg Page.ManageTourneys.Msg
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
      let (route, pageModel, cmd) = updateRoute url model.key
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

    (VoteMsg vMsg, VoteModel vModel) ->
      let (newModel, newCmd) = Page.Vote.update vMsg vModel
      in ({model | pageModel = VoteModel newModel}, Cmd.map VoteMsg newCmd)

    (ManageTourneysMsg mMsg, ManageTourneysModel mModel) ->
      let (newModel, newCmd) = Page.ManageTourneys.update mMsg mModel
      in ({model | pageModel = ManageTourneysModel newModel}, Cmd.map ManageTourneysMsg newCmd)

    (_, _) ->
      let _ = Debug.log "error" "could not match msg/model"
      in (model, Cmd.none)



updateRoute: Url -> Nav.Key -> (Route, PageModel, Cmd Msg)
updateRoute url key =
    let route = Route.parseRoute url
    in
      case Route.parseRoute url of
        Bracket maybeLink ->
          case maybeLink of
            Nothing -> (NotFound, NotFoundModel, Cmd.none)
            Just link ->
              let (bracketModel, bracketCmd) = Page.Bracket.init link
              in (route, BracketModel bracketModel, Cmd.map BracketMsg bracketCmd)

        ManageTourneys maybeLink ->
          case maybeLink of
            Nothing -> (NotFound, NotFoundModel, Cmd.none)
            Just link ->
              let (manageModel, manageCmd) = Page.ManageTourneys.init link
              in (route, ManageTourneysModel manageModel, Cmd.map ManageTourneysMsg manageCmd)

        CreateTourney ->
          let (createModel, createCmd) = Page.CreateTourney.init
          in (route, CreateTourneyModel createModel, Cmd.map CreateTourneyMsg createCmd)

        Home ->
          let (homeModel, homeCmd) = Page.Home.init key
          in (Home, HomeModel homeModel, Cmd.map HomeMsg homeCmd)

        Vote maybeCode maybeName->
          case (maybeCode, maybeName) of
            (Just code, Just name) ->
              let (voteModel, voteCmd) = Page.Vote.init code name
              in (Vote maybeCode maybeName, VoteModel voteModel, Cmd.map VoteMsg voteCmd)
            (_, _) -> (NotFound, NotFoundModel, Cmd.none)

        NotFound -> (NotFound, NotFoundModel, Cmd.none)



subscriptions: Model -> Sub Msg
subscriptions model =
  case model.pageModel of
    BracketModel bracketModel -> Sub.map BracketMsg <| Page.Bracket.subscriptions bracketModel
    VoteModel voteModel -> Sub.map VoteMsg <| Page.Vote.subscriptions voteModel
    _ -> Sub.none



view: Model -> Document Msg
view model =
  { title = "Vote for Stuff"
  , body =
    [ pageContent model
    ]
  }

pageContent : Model -> Html Msg
pageContent model =
  case model.pageModel of
    HomeModel homeModel -> Html.map HomeMsg <| Page.Home.view homeModel
    BracketModel bracketModel -> Html.map BracketMsg <| Page.Bracket.view bracketModel
    CreateTourneyModel createModel -> Html.map CreateTourneyMsg <| Page.CreateTourney.view createModel
    VoteModel voteModel -> Html.map VoteMsg <| Page.Vote.view voteModel
    ManageTourneysModel manageModel -> Html.map ManageTourneysMsg <| Page.ManageTourneys.view manageModel
    NotFoundModel -> h1 [] [ text "404 Page Not Found" ]
