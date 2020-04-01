module PhysicalQuantities.Length exposing (length)
import PhysicalQuantities.PhysicalQuantity exposing (Conversion(..), UnitName, UnitSymbol, UnitAliases, Quantity, QuantityDescription, Unit, Prefix(..), basicUnitsWithAllPrefixes, multiplication)

import Length as LibLength exposing (inMeters)

-- SI
meter = lengthUnit "meter" "m" ["meters", "metre"] (multiplication 1)
otherSi = basicUnitsWithAllPrefixes meter

-- Imperial
inch = unitFromLib LibLength.inch "inch" "\"" ["inches", "in"]
foot = unitFromLib LibLength.foot "foot" "'" ["feet", "ft"]
yard = unitFromLib LibLength.yard "yard" "y" ["yards"]
mile = unitFromLib LibLength.mile "mile" "mi" ["miles", "imperial mile"]
imperial = [ inch, foot, yard, mile ]

-- Other
au = unitFromLib LibLength.astronomicalUnit "astronomical unit" "AU" ["astronomical units", "au", "ua"]
lightYear = unitFromLib LibLength.lightYear "light year" "ly" ["light years"]
parsec = unitFromLib LibLength.parsec "parsec " "pc" ["parsecs"]
otherUnits = [au, lightYear, parsec]

lengthQuantity : Quantity
lengthQuantity = "length"

lengthUnit : UnitName -> UnitSymbol -> UnitAliases -> Conversion -> Unit
lengthUnit name symbol aliases conversion =
    Unit name lengthQuantity symbol aliases conversion

length : QuantityDescription
length = 
    { quantity = lengthQuantity
    , baseSiUnit = meter
    , siUnit = meter
    , alternateUnits = otherSi ++ imperial ++ otherUnits
    }

unitFromLib : LibLength.Length -> UnitName -> UnitSymbol -> UnitAliases -> Unit
unitFromLib lengthVal name symbol aliases =
    Unit name lengthQuantity symbol aliases (multiplication (inMeters lengthVal))
