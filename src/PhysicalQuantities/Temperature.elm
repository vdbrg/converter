module PhysicalQuantities.Temperature exposing (temperature)

import PhysicalQuantities.PhysicalQuantity exposing (Conversion(..), UnitName, UnitSymbol, UnitAliases, Quantity, QuantityDescription, Unit, Prefix(..), basicUnitsWithAllPrefixes, multiplication)

import Temperature as LibTemp exposing (inKelvins, kelvins)

kelvin = tempUnit "degree kelvin" "K" ["degrees kelvin", "kelvin"] (multiplication 1)
celsius = unitFromLib LibTemp.degreesCelsius LibTemp.inDegreesCelsius "degree celsius" "°C" ["C", "degrees celsius", "celsius"]
fahrenheit = unitFromLib LibTemp.degreesFahrenheit LibTemp.inDegreesFahrenheit "degree fahrenheit" "°F" ["F" ,"degrees fahrenheit", "fahrenheit"]

tempQuantity : Quantity
tempQuantity = "temperature"

tempUnit : UnitName -> UnitSymbol -> UnitAliases -> Conversion -> Unit
tempUnit name symbol aliases conversion =
    Unit name tempQuantity symbol aliases conversion

temperature : QuantityDescription
temperature = 
    { quantity = tempQuantity
    , baseSiUnit = kelvin
    , siUnit = kelvin
    , alternateUnits = [celsius, fahrenheit]
    }

unitFromLib : (Float -> LibTemp.Temperature) -> (LibTemp.Temperature -> Float) -> UnitName -> UnitSymbol -> UnitAliases -> Unit
unitFromLib toTemp fromTemp name symbol aliases =
    Unit name tempQuantity symbol aliases (SiConversion (toTemp >> inKelvins) (kelvins >> fromTemp))
