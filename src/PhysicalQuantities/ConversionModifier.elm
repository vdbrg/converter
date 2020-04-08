module PhysicalQuantities.ConversionModifier exposing (..)

import PhysicalQuantities.PhysicalQuantity exposing (QuantityDescription)

type alias ModifierType = String
type alias ConversionModifier =
    { modifierType : ModifierType
    , name : String
    , aliases : List String
    , fromQuantity : QuantityDescription
    , toQuantity : QuantityDescription
    , siUnitConversion : Float -> Float
    }
