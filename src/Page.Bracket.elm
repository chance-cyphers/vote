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
    [ horizontalLine 30 130
    , horizontalLine 30 210
    , horizontalLine 30 290
    , horizontalLine 30 370
    , horizontalLine 30 450
    , horizontalLine 30 530
    , horizontalLine 30 610
    , horizontalLine 30 690
    , horizontalLine 200 170
    , horizontalLine 200 330
    , horizontalLine 200 490
    , horizontalLine 200 650
    , horizontalLine 370 260
    , horizontalLine 370 570
    , horizontalLine 540 420
    , horizontalLine 1400 130
    , horizontalLine 1400 210
    , horizontalLine 1400 290
    , horizontalLine 1400 370
    , horizontalLine 1400 450
    , horizontalLine 1400 530
    , horizontalLine 1400 610
    , horizontalLine 1400 690
    , horizontalLine 1230 170
    , horizontalLine 1230 330
    , horizontalLine 1230 490
    , horizontalLine 1230 650
    , horizontalLine 1060 260
    , horizontalLine 1060 570
    , horizontalLine 890 420
    , horizontalLine 715 370
    , verticalLine 200 130 80
    , verticalLine 200 290 80
    , verticalLine 200 450 80
    , verticalLine 200 610 80
    , verticalLine 370 170 160
    , verticalLine 370 490 160
    , verticalLine 540 260 310
    , verticalLine 1400 130 80
    , verticalLine 1400 290 80
    , verticalLine 1400 450 80
    , verticalLine 1400 610 80
    , verticalLine 1230 170 160
    , verticalLine 1230 490 160
    , verticalLine 1060 260 310
    , verticalLine 888 370 50
    , verticalLine 712 370 50
    ]


horizontalLine: Int -> Int -> Svg.Svg Msg
horizontalLine x y =
  line
    [ x1 <| String.fromInt x
    , x2 <| String.fromInt (x + 170)
    , y1 <| String.fromInt y
    , y2 <| String.fromInt y
    , stroke "#fff"
    , strokeWidth "3"
    , fill "none"
    ]
    []

verticalLine: Int -> Int -> Int -> Svg.Svg Msg
verticalLine x y size =
  line
    [ x1 <| String.fromInt x
    , x2 <| String.fromInt x
    , y1 <| String.fromInt y
    , y2 <| String.fromInt (y + size)
    , stroke "#fff"
    , strokeWidth "3"
    , fill "none"
    ]
    []


render16: Maybe (List Contestant) -> List (Svg.Svg Msg)
render16 contestants =
  case contestants of
    Nothing -> []
    Just val -> List.indexedMap renderContestantBracket val

renderContestantBracket: Int -> Contestant -> Svg.Svg Msg
renderContestantBracket index contestant =
  tspan [ x "55", y "120"] [ Svg.text contestant.name ]
