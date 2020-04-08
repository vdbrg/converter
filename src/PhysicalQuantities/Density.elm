module PhysicalQuantities.Density exposing (densities)

import PhysicalQuantities.ConversionModifier exposing (ConversionModifier, ModifierType)
import PhysicalQuantities.Mass exposing (mass)
import PhysicalQuantities.Volume exposing (volume)

water = density "water" ["H2O"] 1000
milk = density "milk" [] 1100
yoghurt = density "yoghurt" [] 1100
flour = density "flour" ["wheat flour"] 510
sugar = density "sugar" [] 510
butter = density "butter" [] 960
rice = density "rice" ["white rice"] 870
oats = density "oats" [] 660

baking = [water, milk ,flour, sugar, butter]

densityType : ModifierType 
densityType = "density"

density : String -> List String -> Float -> ConversionModifier
density name aliases kgPerCubicMeter =
    { modifierType = densityType
    , name = name
    , aliases = aliases
    , fromQuantity = mass
    , toQuantity = volume
    , siUnitConversion = \x -> x*kgPerCubicMeter
    }

densities : List ConversionModifier
densities = baking