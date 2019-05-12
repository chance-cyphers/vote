module Page.Bracket exposing (..)


import Browser
import Html exposing (Html)
import Svg exposing (Svg, rect, svg, text_, tspan)
import Svg.Attributes exposing (dy, fill, fontSize, height, preserveAspectRatio, viewBox, width, x, y)



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
        { name = "Bob"
        }
        ,
        { name = "Charles"
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
    , viewBox "0 0 1600 900"
    , preserveAspectRatio "none"
    ]
    [ rect
      [ width "100%"
      , height "100%"
      , fill "#609"
      ]
      []
     ,text_
      [ x "80"
      , y "80"
      , fontSize "20"
      , fill "white"
      ]
      (render16 model.roundOf16)
    ]


render16: Maybe (List Contestant) -> List (Svg.Svg Msg)
render16 contestants =
  case contestants of
    Nothing -> []
    Just val -> List.indexedMap renderContestant val

renderContestant: Int -> Contestant -> Svg.Svg Msg
renderContestant index contestant =
  tspan [ x (String.fromInt (index * 10)), dy "1.2em" ] [ Svg.text contestant.name ]
