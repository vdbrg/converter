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

import PhysicalQuantities.PhysicalQuantity exposing (QuantityDescription, Quantity, Measurement(..), convert)
import PhysicalQuantities.ConversionParser exposing (parseConversionRequest, ConversionRequest, emptyRequest)
import PhysicalQuantities.KnownQuantities exposing (quantities, getQuantityDescription)

import String
import Maybe.Extra as MaybeE
import List.Extra as ListE
import Dict exposing (Dict)
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
parseInput input = parseConversionRequest quantities input

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
        maybeQuantities = [ request.fromUnit, request.toUnit ] |> List.map (Maybe.map .quantity)
        recogQuantities = MaybeE.values maybeQuantities |> ListE.unique
    in
        case recogQuantities of
            [] -> text "No quantity found"
            [quantity] -> 
                let
                   conv = boxedText [] <| "Convert " ++ quantity
                   quantityDescription = getQuantityDescription quantity
                   units = mappend request.fromUnit request.toUnit
                in
                    case units of
                        Nothing -> err <| "Need from and to unit"
                        Just (fromUnit, toUnit) -> 
                            case quantityDescription of
                                Nothing -> err <| "Unknown quantity " ++ quantity
                                Just desc ->
                                    case request.amount of
                                        Nothing -> conv
                                        Just amount -> 
                                            let
                                                fromView = boxedText [] <| formatNumber amount ++ " " ++ fromUnit.symbol
                                                eqView = boxedText [] "="
                                                toValue = convert desc (Measurement amount fromUnit) toUnit
                                                toValueView = boxedText [] <| formatNumber toValue ++ " " ++ toUnit.symbol
                                            in
                                                row [spacing 5] [ conv, fromView, eqView, toValueView]
            qs ->  err <| "Found multiple quantities: "  ++  String.join ", " qs

formatNumber : Float -> String
formatNumber n =
    String.fromFloat n

mappend : Maybe a -> Maybe b -> Maybe (a,b)
mappend va vb =
    Maybe.map2 Tuple.pair va vb 

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