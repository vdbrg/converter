module PhysicalQuantities.ConversionParser exposing (..)

import PhysicalQuantities.PhysicalQuantity exposing (Unit, QuantityDescription, knownUnits)
import PhysicalQuantities.ConversionModifier exposing (ConversionModifier)
import Dict exposing (Dict)
import List
import Parser exposing (..)
import Parser.Dict as DictParser
import Result

type alias ParseResult = 
    { amount : Maybe Float
    , fromUnit : Maybe Unit
    , conversionModifier : Maybe ConversionModifier
    , toWord : Maybe String
    , toUnit : Maybe Unit
    }

type alias FromUnit = Unit
type alias ToUnit = Unit
type ConversionRequest
    = None
    | OnlyFrom FromUnit
    | InconsistentQuantities FromUnit ToUnit
    | InconsistentModifier FromUnit ConversionModifier ToUnit
    | SimpleRequest Float FromUnit ToUnit
    | WithModifier Float FromUnit ConversionModifier ToUnit

emptyRequest : ConversionRequest
emptyRequest = None


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
    oneOf [ backtrackable fraction, backtrackable float ] |> maybeParser

fraction : Parser Float
fraction =
    succeed (/)
        |= float
        |. whitespace
        |. symbol "/"
        |. whitespace
        |= float

dictParser : Dict String a -> Parser (Maybe a)
dictParser xs = 
    DictParser.fromDict xs |> maybeParser


unitsFromNames : List QuantityDescription -> Dict String Unit
unitsFromNames quantities =
    List.concatMap knownUnits quantities 
    |> List.concatMap (\u -> unitStrings u |> List.map (\s -> (s, u))) 
    |> Dict.fromList

conversionModsFromNames : List ConversionModifier -> Dict String ConversionModifier
conversionModsFromNames modifiers =
    modifiers
    |> List.concatMap (\m -> m.name :: m.aliases |> List.map (\s -> (s, m)))
    |> Dict.fromList

unitStrings : Unit -> List String
unitStrings u =
    [ u.name, u.symbol ] ++ u.aliases

quantityParser : List QuantityDescription -> List ConversionModifier -> Parser ConversionRequest
quantityParser quantities conversionModifiers =
    let 
        unitDict = unitsFromNames quantities
        unitParser = dictParser unitDict
        conversionModDict = conversionModsFromNames conversionModifiers
        conversionModParser = dictParser conversionModDict
    in
    map conversionRequest <| succeed ParseResult
        |. whitespace
        |. maybeParser from
        |. whitespace
        |= amount
        |. whitespace
        |= unitParser
        |. whitespace
        |= conversionModParser
        |. whitespace
        |= to
        |. whitespace
        |= unitParser
        |. whitespace
        |. end
    
parseConversionRequest : List QuantityDescription -> List ConversionModifier -> String -> Result String ConversionRequest
parseConversionRequest quantities conversionModifiers input =
    run (quantityParser quantities conversionModifiers) input 
    |> Result.mapError deadEndsToString


conversionRequest : ParseResult -> ConversionRequest 
conversionRequest request = 
    case request.fromUnit of
        Nothing -> None
        Just fromUnit ->
            case request.toUnit of
                Nothing -> OnlyFrom fromUnit
                Just toUnit ->
                    case request.amount of 
                        Nothing -> OnlyFrom fromUnit
                        Just value ->
                            case request.conversionModifier of
                                Nothing -> 
                                    if fromUnit.quantity == toUnit.quantity then
                                        SimpleRequest value fromUnit toUnit
                                    else
                                        InconsistentQuantities fromUnit toUnit
                                Just modifier ->
                                    let
                                        modifierFromQuantity = modifier.fromQuantity.quantity
                                        modifierToQuantity = modifier.toQuantity.quantity
                                    in
                                    if modifierFromQuantity /= toUnit.quantity || modifierToQuantity /= fromUnit.quantity then
                                        InconsistentModifier fromUnit modifier toUnit
                                    else
                                        WithModifier value fromUnit modifier toUnit
