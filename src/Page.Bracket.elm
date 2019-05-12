module Page.Bracket exposing (..)


import Browser
import Html exposing (Html)
import Svg exposing (svg, text_, tspan)
import Svg.Attributes exposing (dy, fontSize, height, viewBox, width, x, y)



main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



type alias Model =
  {
  }

init: () -> (Model, Cmd Msg)
init _ =
  ( Model
  , Cmd.none
  )



type Msg = Hi

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( Model
  , Cmd.none
  )



subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.none



view: Model -> Html Msg
view model =
  svg
    [ width "500"
    , height "500"
    , viewBox "0 0 500 500"
    ]
    [ text_
      [ x "80"
      , y "80"
      , fontSize "20"
      ]
      [ tspan [ x "0", dy "1.2em" ] [ Svg.text "cool placeholder"]
      ]
    ]


