module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, h1, text)
import Page.Bracket exposing (Model, init)
import Page.CreateTourney
import Page.Tourney
import Page.Home
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
  | TourneyModel Page.Tourney.Model
  | CreateTourneyModel Page.CreateTourney.Model
  | VoteModel Page.Vote.Model
  | NotFoundModel


-- INIT


init: () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url navKey =
  let
    (route, model, cmd) = updateRoute url
  in (Model route model navKey, cmd)



type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | BracketMsg Page.Bracket.Msg
  | TourneyMsg Page.Tourney.Msg
  | VoteMsg Page.Vote.Msg
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

    (TourneyMsg tMsg, TourneyModel tModel) ->
      let (newModel, newCmd) = Page.Tourney.update tMsg tModel
      in ({model | pageModel = TourneyModel newModel}, Cmd.map TourneyMsg newCmd)

    (VoteMsg vMsg, VoteModel vModel) ->
      let (newModel, newCmd) = Page.Vote.update vMsg vModel
      in ({model | pageModel = VoteModel newModel}, Cmd.map VoteMsg newCmd)

    (_, _) ->
      let _ = Debug.log "error" "could not match msg/model"
      in (model, Cmd.none)



updateRoute: Url -> (Route, PageModel, Cmd Msg)
updateRoute url =
    let route = Route.parseRoute url
    in
      case Route.parseRoute url of
        Bracket maybeLink->
          case maybeLink of
            Nothing -> (NotFound, NotFoundModel, Cmd.none)
            Just link ->
              let (bracketModel, bracketCmd) = Page.Bracket.init link
              in (route, BracketModel bracketModel, Cmd.map BracketMsg bracketCmd)

        CreateTourney ->
          let (createModel, createCmd) = Page.CreateTourney.init
          in (route, CreateTourneyModel createModel, Cmd.map CreateTourneyMsg createCmd)

        Home ->
          let (homeModel, homeCmd) = Page.Home.init
          in (Home, HomeModel homeModel, Cmd.map HomeMsg homeCmd)

        Tourney maybeLink ->
          case maybeLink of
            Nothing -> (NotFound, NotFoundModel, Cmd.none)
            Just link ->
              let (tourneyModel, tourneyCmd) = Page.Tourney.init link
              in (Tourney maybeLink, TourneyModel tourneyModel, Cmd.map TourneyMsg tourneyCmd)

        Vote maybeLink ->
          case maybeLink of
            Nothing -> (NotFound, NotFoundModel, Cmd.none)
            Just link ->
              let (voteModel, voteCmd) = Page.Vote.init link
              in (Vote maybeLink, VoteModel voteModel, Cmd.map VoteMsg voteCmd)

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
    VoteModel voteModel -> Html.map VoteMsg <| Page.Vote.view voteModel
    NotFoundModel -> h1 [] [ text "404 Page Not Found" ]
