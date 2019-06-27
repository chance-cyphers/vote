module Route exposing (..)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string)
import Url.Parser.Query as Query

type Route
    = Home
    | Bracket
    | CreateTourney
    | Tourney (Maybe String)
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
        , Parser.map Tourney (s "tourney" <?> Query.string "link")
        , Parser.map CreateTourney (s "create")
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


