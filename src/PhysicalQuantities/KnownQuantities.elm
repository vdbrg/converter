module PhysicalQuantities.KnownQuantities exposing (..)

import PhysicalQuantities.PhysicalQuantity exposing (QuantityDescription, Quantity)
import PhysicalQuantities.Mass exposing (mass)
import PhysicalQuantities.Length exposing (length)
import PhysicalQuantities.Volume exposing (volume)

import Dict exposing (Dict)

quantities : List QuantityDescription
quantities = 
    [ mass
    , length
    , volume
    ]

quantityDescriptionMap : Dict Quantity QuantityDescription
quantityDescriptionMap = 
    List.map (\q -> (q.quantity, q)) quantities |> Dict.fromList

getQuantityDescription : Quantity -> Maybe QuantityDescription
getQuantityDescription quantity =
    Dict.get quantity quantityDescriptionMap
