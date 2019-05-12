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
  , key: Nav.Key
  }

init: () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ _ key =
  ( Model "Hello world" key
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
  { title = "Hi"
  , body =
      [ div []
        (List.map renderARow [1, 2, 3])
      ]
  }


renderARow _ =
    h1 [] [ text "Hello world" ]
