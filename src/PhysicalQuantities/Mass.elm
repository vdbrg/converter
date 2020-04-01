module PhysicalQuantities.Mass exposing (mass)

import PhysicalQuantities.PhysicalQuantity exposing (Conversion(..), Quantity, QuantityDescription, Unit, UnitName, UnitSymbol, UnitAliases, Prefix(..), basicUnitsWithAllPrefixes, multiplication)
import Mass as LibMass exposing (inKilograms)

-- SI
gram = massUnit "gram" "g" ["grams", "gramme", "grammes"] (multiplication 0.001)
kilogram = massUnit "kilogram" "kg" ["kilograms", "kilo", "kilogramme", "kilogrammes"] (multiplication 1)
metricTon = unitFromLib LibMass.metricTon "ton" "t" ["tons", "tonne", "tonnes"]
otherSi = basicUnitsWithAllPrefixes gram 

-- Imperial
avoirDupoisOunce = unitFromLib LibMass.ounce "ounce" "oz" ["ounces", "avoirdupois ounce"]
pound = unitFromLib LibMass.pound "pound" "lbs" ["pounds"]
 
-- Other
dalton = massUnit "dalton" "u" ["daltons", "atomic mass unit", "Da"] (multiplication (1.66053906660*10^(-27)))
solarMass = massUnit "sun" "M0" ["suns", "solar mass"] (multiplication (1.98847*10^(30)))

massQuantity : Quantity
massQuantity = "mass" 

mass : QuantityDescription
mass = 
    { quantity = massQuantity
    , baseSiUnit = gram
    , siUnit = kilogram
    , alternateUnits = 
        [ avoirDupoisOunce
        , pound
        , metricTon
        , dalton
        , solarMass
        ] ++ otherSi
    }
massUnit : UnitName -> UnitSymbol -> UnitAliases -> Conversion -> Unit
massUnit name symbol aliases conversion =
    Unit name massQuantity symbol aliases conversion

unitFromLib : LibMass.Mass -> UnitName -> UnitSymbol -> UnitAliases -> Unit
unitFromLib massVal name symbol aliases =
    Unit name massQuantity symbol aliases (multiplication (inKilograms massVal))
