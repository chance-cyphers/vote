module Route exposing (..)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)
import Url.Parser.Query as Query

type Route
    = Home
    | Bracket (Maybe String)
    | CreateTourney
    | ManageTourneys (Maybe String)
    | Vote (Maybe String) (Maybe String)
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
        , Parser.map Bracket (s "bracket" <?> Query.string "link")
        , Parser.map Vote (s "vote" <?> Query.string "code" <?> Query.string "name")
        , Parser.map ManageTourneys (s "manage" <?> Query.string "get-link")
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


