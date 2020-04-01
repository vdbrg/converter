module PhysicalQuantities.Volume exposing (volume)

import PhysicalQuantities.PhysicalQuantity exposing (Conversion(..), UnitName, UnitSymbol, UnitAliases, Quantity, QuantityDescription, Unit, Prefix(..), multiplication, basicUnitsWithAllPrefixes, unitsWithAllPrefixes)

import Volume as LibVolume exposing (inCubicMeters)

-- SI
liter = unitFromLib LibVolume.liter "liter" "l" ["liters", "litre", "litres"]
cubicMeter = volumeUnit "cubic meter" "m³" ["m3", "cubic meters"] (multiplication 1)

otherSi = [liter] 
    ++ (unitsWithAllPrefixes cubicMeter 3 (\p -> List.concatMap (\a -> [p.symbol ++ "m3", "cubic " ++ p.name ++ "meters", "cubic " ++ p.name ++ "meters"]) cubicMeter.aliases)) 
    ++ basicUnitsWithAllPrefixes liter

-- Imperial
cubicInch = unitFromLib LibVolume.cubicInch "cubic inch" "in³" ["cubic inches", "in3"]
cubicFoot = unitFromLib LibVolume.cubicFoot "cubic foot" "ft³" ["cubic feet", "ft3"]
cubicYard = unitFromLib LibVolume.cubicYard "cubic yard" "yd³" ["cubic yards", "yd3"]
imperial = [ cubicInch, cubicFoot, cubicYard ]

usCup = volumeUnit "cup" "cup" ["us cup", "cups", "us cups"] (multiplication 0.000236588)
teaspoon = volumeUnit "teaspoon" "tsp" ["teaspoons"] (multiplication 0.000004929)
tablespoon = volumeUnit "tablespoon" "tb" ["tablespoons"] (multiplication 0.00001479)
otherUnits = [usCup, teaspoon, tablespoon]

volumeUnit : UnitName -> UnitSymbol -> UnitAliases -> Conversion -> Unit
volumeUnit name symbol aliases conversion =
    Unit name volumeQuantity symbol aliases conversion

volumeQuantity : Quantity
volumeQuantity = "volume"

volume : QuantityDescription
volume = 
    { quantity = volumeQuantity
    , baseSiUnit = cubicMeter
    , siUnit = cubicMeter
    , alternateUnits = otherSi ++ imperial ++ otherUnits
    }

unitFromLib : LibVolume.Volume -> UnitName -> UnitSymbol -> UnitAliases -> Unit
unitFromLib volumeVal name symbol aliases =
    Unit name volumeQuantity symbol aliases (multiplication (inCubicMeters volumeVal))
