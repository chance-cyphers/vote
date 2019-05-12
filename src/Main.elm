module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, text)
import Url



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
    { greeting: String
    }

init: () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ _ _ =
    ( Model "Hello world"
    , Cmd.none
    )



type Msg
  = Hi
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url



update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    ( model
    , Cmd.none
    )



subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none



view: Model -> Document Msg
view model =
  { title = ""
  , body =
      [ div []
        [ h1 [] [ text "Hello world" ] ]
      ]
  }
