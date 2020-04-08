module Main exposing (main)

import Html exposing (Html)
import Html.Events
import Browser

import Element exposing (Attribute, Element, el, text, row, column, alignLeft, alignRight, fill, width, rgb255, spacing, centerX, centerY, padding, htmlAttribute, px, minimum, maximum)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region

import PhysicalQuantities.PhysicalQuantity exposing (Measurement(..), convert)
import PhysicalQuantities.ConversionParser exposing (parseConversionRequest, ConversionRequest(..))
import PhysicalQuantities.KnownQuantities exposing (quantities, conversionModifiers, convertWithModifier)

import String
import Tuple

type alias Model =
    { inputText : String
    , result : Maybe (Result String ConversionRequest)
    }

type Message = ChangeInput String

update : Message -> Model ->  Model
update message model = 
    case message of
        ChangeInput input ->
            { model | inputText = input, result = if String.isEmpty input then Nothing else Just <| parseInput input }

view : Model -> Html Message
view model =
    Element.layout
        [ Font.family [ Font.typeface "Courier" ]
        , Font.size 18
        ]
        ( column [ width fill, centerX, width (fill |> minimum 300 |> maximum 900), spacing 30, padding 10 ]
            [ el h1 (text "Converter")
            , Input.text [ ]
                { text = model.inputText
                , onChange = ChangeInput
                , placeholder = Nothing
                , label = (Input.labelHidden "Conversion input")
                }
            , el [] (viewResult model.result)
            ]
        )

parseInput : String -> Result String ConversionRequest
parseInput input = parseConversionRequest quantities conversionModifiers input

viewResult : Maybe (Result String ConversionRequest) -> Element msg
viewResult maybeResult =
    case maybeResult of
        Nothing -> text ""
        Just result ->
            case result of
                Err failed -> text "?"
                Ok convReq -> viewConversionRequest convReq

viewConversionRequest : ConversionRequest -> Element msg
viewConversionRequest request =
    let
        eqView = boxedText [] "="
    in
        case request of
            None -> text "No quantity found"
            OnlyFrom fromUnit -> text "Need from and to unit"
            InconsistentQuantities fromUnit toUnit -> text <| "Found multiple quantities: "  ++ fromUnit.quantity ++ " and " ++ toUnit.quantity
            InconsistentModifier fromUnit modifier toUnit -> text <| "Can't convert from " ++ fromUnit.quantity ++ " to " ++ toUnit.quantity ++ " with " ++ modifier.modifierType --++ "(" ++ modifierFromQuantity ++ "/" ++ modifierToQuantity ++ ")"
            SimpleRequest amount fromUnit toUnit -> 
                let
                    fromView = boxedText [] <| formatNumber amount ++ " " ++ fromUnit.symbol
                    toValue = convert (Measurement amount fromUnit) toUnit
                    toValueView = boxedText [] <| formatNumber toValue ++ " " ++ toUnit.symbol
                    conv = boxedText [] <| "Convert " ++ fromUnit.quantity
                in
                    row [spacing 5] [ conv, fromView, eqView, toValueView]
            WithModifier amount fromUnit modifier toUnit ->
                let
                    fromView = boxedText [] <| formatNumber amount ++ " " ++ fromUnit.symbol
                    modifierView = boxedText [] <| modifier.name
                    toValue = convertWithModifier modifier (Measurement amount fromUnit) toUnit
                    toValueView = boxedText [] <| formatNumber toValue ++ " " ++ toUnit.symbol
                    conv = boxedText [] <| "Convert " ++ fromUnit.quantity
                in
                    row [spacing 5] [ conv, fromView, modifierView, eqView, toValueView]

formatNumber : Float -> String
formatNumber n =
    String.fromFloat n

err : String -> Element msg
err msg =
    text msg

boxedText : List (Attribute msg) -> String -> Element msg
boxedText attr str
    =  el ([ Border.rounded 5, Border.width 1, padding 10, Border.solid ] ++ attr) <| text str

main =
    Browser.sandbox
        { init = { inputText = "", result = Nothing}
        , update = update
        , view = view
        }

h1 : List (Element.Attribute msg)
h1 = 
    [ Region.heading 1
    , Font.semiBold
    , Font.size 30 
    ]