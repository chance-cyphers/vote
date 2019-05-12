module Page.Bracket exposing (..)


import Browser
import Html exposing (Html)
import Svg exposing (Svg, line, rect, svg, text_, tspan)
import Svg.Attributes exposing (dy, fill, fontSize, height, preserveAspectRatio, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)



main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



type alias Model =
  { title: String
  , roundOf16: Maybe (List Contestant)
  }

type alias Contestant =
  { name: String }

init: () -> (Model, Cmd Msg)
init _ =
  ( { title = ""
    , roundOf16 =
      Just [
        { name = "Toothbrush"
        }
        ,
        { name = "Toothbrush"
        }
      ]
    }
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
  svg
    [ width "100%"
    , height "100%"
    , viewBox "0 0 1600 800"
    , preserveAspectRatio "none"
    ]
    [ rect
      [ width "100%"
      , height "100%"
      , fill "#609"
      ]
      []
    , renderBracket
     ,text_
      [ x "80"
      , y "80"
      , fontSize "16"
      , fill "white"
      ]
      (render16 model.roundOf16)
    ]


renderBracket: Svg.Svg Msg
renderBracket =
  Svg.g
    []
    [ horizontalLine 50 130
    , horizontalLine 50 210
    , horizontalLine 50 290
    , horizontalLine 50 370
    , horizontalLine 50 450
    , horizontalLine 50 530
    , horizontalLine 50 610
    , horizontalLine 50 690
    , horizontalLine 220 170
    , horizontalLine 220 330
    , horizontalLine 220 490
    , horizontalLine 220 650
    , horizontalLine 390 260
    , horizontalLine 390 570
    , horizontalLine 560 420
    , horizontalLine 1440 130
    , horizontalLine 1440 210
    , horizontalLine 1440 290
    , horizontalLine 1440 370
    , horizontalLine 1440 450
    , horizontalLine 1440 530
    , horizontalLine 1440 610
    , horizontalLine 1440 690
    , horizontalLine 1270 170
    , horizontalLine 1270 330
    , horizontalLine 1270 490
    , horizontalLine 1270 650
    , horizontalLine 1100 260
    , horizontalLine 1100 570
    , horizontalLine 930 420
    , horizontalLine 735 370
    ]


horizontalLine: Int -> Int -> Svg.Svg Msg
horizontalLine x y =
  line
    [ x1 <| String.fromInt x
    , x2 <| String.fromInt (x + 130)
    , y1 <| String.fromInt y
    , y2 <| String.fromInt (y)
    , stroke "#fff"
    , strokeWidth "3"
    , fill "none"
    ]
    []


render16: Maybe (List Contestant) -> List (Svg.Svg Msg)
render16 contestants =
  case contestants of
    Nothing -> []
    Just val -> List.indexedMap renderContestant val

renderContestant: Int -> Contestant -> Svg.Svg Msg
renderContestant index contestant =
  tspan [ x "55", y "120"] [ Svg.text contestant.name ]
