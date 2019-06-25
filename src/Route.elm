module Route exposing (Route, parseRoute)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)

type Route
    = Home
    | Bracket
    | NotFound


parseRoute : Url -> Route
parseRoute url =
  urlFragmentToPath url
      |> Parser.parse parser
      |> Maybe.withDefault NotFound


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Bracket (s "bracket")
        ]

urlFragmentToPath : Url -> Url
urlFragmentToPath url =
    { url
        | path = Maybe.withDefault "" url.fragment
        , fragment = Nothing
    }
        |> Url.toString
        |> Url.fromString
        |> Maybe.withDefault url


