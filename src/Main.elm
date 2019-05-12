module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, div, h1, text)



main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



type alias Model =
    { greeting: String
    }

init: () -> (Model, Cmd Msg)
init _ =
    ( Model "Hello world"
    , Cmd.none
    )



type Msg = Hi

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    ( model
    , Cmd.none
    )



subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none



view: Model -> Html Msg
view model =
    div[]
      [ h1 [] [ text model.greeting ]
      ]

