module PhysicalQuantities.KnownQuantities exposing (..)

import PhysicalQuantities.PhysicalQuantity exposing (QuantityDescription, Quantity, Measurement(..), Conversion(..), Unit, convert)
import PhysicalQuantities.ConversionModifier exposing (ConversionModifier)
import PhysicalQuantities.Mass exposing (mass)
import PhysicalQuantities.Length exposing (length)
import PhysicalQuantities.Volume exposing (volume)
import PhysicalQuantities.Density exposing (densities)

import Dict exposing (Dict)

quantities : List QuantityDescription
quantities = 
    [ mass
    , length
    , volume
    ]

conversionModifiers : List ConversionModifier
conversionModifiers =
    densities

quantityDescriptionMap : Dict Quantity QuantityDescription
quantityDescriptionMap = 
    List.map (\q -> (q.quantity, q)) quantities |> Dict.fromList

getQuantityDescription : Quantity -> Maybe QuantityDescription
getQuantityDescription quantity =
    Dict.get quantity quantityDescriptionMap

convertWithModifier : ConversionModifier -> Measurement -> Unit -> Float
convertWithModifier modifier (Measurement amount unit) toUnit =
    let
        (SiConversion toSi _) = unit.conversion
        (SiConversion _ fromSi) =  toUnit.conversion
    in
        toSi amount |> modifier.siUnitConversion |> fromSi

convert : Measurement -> Unit -> Float
convert (Measurement amount unit) toUnit =
    let
        (SiConversion toSi _) = unit.conversion
        (SiConversion _ fromSi) =  toUnit.conversion
    in
        toSi amount |> fromSi
