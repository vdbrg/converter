module PhysicalQuantities.PhysicalQuantity exposing (..)

type alias ToSiUnit = Float -> Float
type alias FromSiUnit = Float -> Float
type Conversion
    = SiConversion ToSiUnit FromSiUnit
    
type alias Quantity = String
type alias UnitName = String
type alias UnitSymbol = String
type alias UnitAliases = List UnitName

type alias Unit =
    { name : UnitName
    , quantity : Quantity
    , symbol : UnitSymbol
    , aliases : UnitAliases
    , conversion : Conversion
    }

type alias QuantityDescription =
    { quantity : Quantity
    , baseSiUnit : Unit
    , siUnit : Unit
    , alternateUnits : List Unit
    }
type Prefix
    = Nano
    | Micro
    | Milli
    | Centi
    | None
    | Deci
    | Hecto
    | Kilo
    | Mega
    | Giga

type alias PrefixProp = 
    { name : String
    , symbol : String
    , factor : Float
    }

type Measurement = 
    Measurement Float Unit

prefixProps : Prefix -> PrefixProp
prefixProps prefix =
    case prefix of
        Nano -> PrefixProp "nano" "n" <| 10^(-9)
        Micro -> PrefixProp "micro" "mu" <| 10^(-6)
        Milli -> PrefixProp "milli" "m" <| 10^(-3)
        Centi -> PrefixProp "centi" "c" 0.1
        None -> PrefixProp "" "" 1
        Deci -> PrefixProp "deci" "d" 10
        Hecto -> PrefixProp "hecto" "h" 100
        Kilo -> PrefixProp "kilo" "k" <| 10^3
        Mega -> PrefixProp "mega" "M" <| 10^6
        Giga -> PrefixProp "giga" "G" <| 10^9

-- base SI is without prefix
unitWithPrefix : Unit -> Prefix -> Int -> UnitAliases -> Unit
unitWithPrefix baseSiUnit prefix exponent aliases =
    let 
        prefixProp = prefixProps prefix
    in
    { name = prefixProp.name ++ baseSiUnit.name
    , quantity = baseSiUnit.quantity
    , symbol = prefixProp.symbol ++ baseSiUnit.symbol
    , aliases = aliases
    , conversion = conversionFromPrefix baseSiUnit prefix exponent
    }

allPrefixes : List Prefix
allPrefixes = [Nano, Micro, Milli, Centi, Deci, Hecto, Kilo, Mega, Giga]

basicUnitsWithAllPrefixes : Unit -> List Unit
basicUnitsWithAllPrefixes baseSiUnit =
    unitsWithAllPrefixes baseSiUnit 1 (\p -> List.concatMap (\a -> [p.symbol ++ a, p.name ++ a]) baseSiUnit.aliases)

unitsWithAllPrefixes : Unit -> Int -> (PrefixProp -> UnitAliases) -> List Unit
unitsWithAllPrefixes baseSiUnit exponent toAliases =
    List.map (\p -> unitWithPrefix baseSiUnit p exponent (toAliases <| prefixProps p)) allPrefixes

conversionFromPrefix : Unit -> Prefix -> Int -> Conversion
conversionFromPrefix baseSiUnit prefix exponent =
    let
        prefixProp = prefixProps prefix
        (SiConversion baseToSi siToBase) = baseSiUnit.conversion
        floatExp = toFloat exponent
    in
        SiConversion (\x -> baseToSi x*prefixProp.factor^floatExp) (\x -> (siToBase x)/prefixProp.factor^floatExp)

    
convertToSi : QuantityDescription -> Measurement -> Float
convertToSi quantity (Measurement amount unit) = 
    case unit.conversion of
        (SiConversion toCalc fromCalc) -> toCalc amount

knownUnits : QuantityDescription -> List Unit
knownUnits quantity =
    [quantity.baseSiUnit, quantity.siUnit] ++ quantity.alternateUnits

multiplication : Float -> Conversion
multiplication multiplier = SiConversion (\x -> x*multiplier) (\x -> x/multiplier)
