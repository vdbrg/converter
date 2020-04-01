module PhysicalQuantities.ConversionParser exposing (..)

import PhysicalQuantities.PhysicalQuantity exposing (Unit, Quantity, QuantityDescription, knownUnits)
import Dict exposing (Dict)
import List
import Parser exposing (..)
import Parser.Dict as DictParser
import Result

type alias ConversionRequest = 
    { amount : Maybe Float
    , fromUnit : Maybe Unit
    , toWord : Maybe String
    , toUnit : Maybe Unit
    }

emptyRequest : ConversionRequest
emptyRequest = ConversionRequest Nothing Nothing Nothing Nothing


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ')

from : Parser ()
from = 
    oneOf [ symbol "from", symbol "convert" ]

to : Parser (Maybe String)
to = 
    let
        toParser = oneOf [ symbol "to", symbol "in" ]
    in
        map (\x -> "to") toParser |> maybeParser
    

maybeParser : Parser a -> Parser (Maybe a)
maybeParser parser =
    oneOf 
        [ succeed Just
            |= parser
        , succeed Nothing
        ]

amount : Parser (Maybe Float)
amount = 
    maybeParser float

unit : Dict String Unit -> Parser (Maybe Unit)
unit units = 
    DictParser.fromDict units |> maybeParser

unitsFromNames : List QuantityDescription -> Dict String Unit
unitsFromNames quantities =
    List.concatMap knownUnits quantities 
    |> List.concatMap (\u -> unitStrings u |> List.map (\s -> (s, u))) 
    |> Dict.fromList

unitStrings : Unit -> List String
unitStrings u =
    [ u.name, u.symbol ] ++ u.aliases

quantityParser : List QuantityDescription -> Parser ConversionRequest
quantityParser quantities =
    let 
        unitDict = unitsFromNames quantities
        unitParser = unit unitDict
    in
    succeed ConversionRequest
        |. whitespace
        |. maybeParser from
        |. whitespace
        |= amount
        |. whitespace
        |= unitParser
        |. whitespace
        |= to
        |. whitespace
        |= unitParser
        |. whitespace
        |. end

parseConversionRequest : List QuantityDescription -> String -> Result String ConversionRequest
parseConversionRequest quantities input =
    run (quantityParser quantities) input 
    |> Result.mapError deadEndsToString

