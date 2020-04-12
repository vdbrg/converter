module Main exposing (main)

import Html exposing (Html)
import Html.Events
import Browser

import Element exposing (Attribute, Element, Color, el, text, row, column, alignLeft, alignRight, fill, width, rgb255, spacing, centerX, centerY, padding, htmlAttribute, px, minimum, maximum)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region

import PhysicalQuantities.PhysicalQuantity exposing (Measurement(..), Unit)
import PhysicalQuantities.ConversionParser exposing (parseConversionRequest, ConversionRequest(..))
import PhysicalQuantities.KnownQuantities exposing (quantities, conversionModifiers, convert, convertWithModifier)
import PhysicalQuantities.ConversionModifier exposing (ConversionModifier)

import String

type alias Model =
    { inputText : String
    , result : Maybe (Result String ConversionRequest)
    , pinned : List PinnedItem
    }

type Message 
    = ChangeInput String
    | PinItem
    | UnpinItem Int

type ConversionState 
    = InvalidConversion String
    | SimpleConversion Float Unit Unit Float
    | ConversionWithModifier Float Unit ConversionModifier Unit Float

type alias PinnedItem = ConversionState

update : Message -> Model ->  Model
update message model = 
    case message of
        ChangeInput input ->
            { model | inputText = input, result = if String.isEmpty input then Nothing else Just <| parseInput input }
        PinItem ->
            case model.result of
                Nothing -> model
                Just result ->
                    case result of
                        Err _ -> model
                        Ok conversionRequest -> { model | pinned = appendToList (requestToState conversionRequest) model.pinned}
        UnpinItem idx -> { model | pinned = removeFromList idx model.pinned }

appendToList : a -> List a -> List a
appendToList item list =
    item :: list

removeFromList : Int ->  List a -> List a
removeFromList  index list = 
    List.take index list ++ List.drop (index + 1) list


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
            , viewPinnedItems model.pinned
            ]
        )

viewPinnedItems : List PinnedItem -> Element Message
viewPinnedItems pinnedItems =
    column [ spacing 5 ] (List.indexedMap (\i s -> viewConversionState (Just i) s) pinnedItems)


parseInput : String -> Result String ConversionRequest
parseInput input = parseConversionRequest quantities conversionModifiers input

viewResult : Maybe (Result String ConversionRequest) -> Element Message
viewResult maybeResult =
    case maybeResult of
        Nothing -> text ""
        Just result ->
            case result of
                Err failed -> text "?"
                Ok convReq -> convReq |> requestToState |> viewConversionState Nothing

requestToState : ConversionRequest -> ConversionState
requestToState request =
    case request of
        None -> InvalidConversion "No quantity found"
        OnlyFrom _ -> InvalidConversion "Need from and to unit"
        InconsistentQuantities fromUnit toUnit -> InvalidConversion <| "Found multiple quantities: "  ++ fromUnit.quantity ++ " and " ++ toUnit.quantity
        InconsistentModifier fromUnit modifier toUnit -> InvalidConversion <| "Can't convert from " ++ fromUnit.quantity ++ " to " ++ toUnit.quantity ++ " with " ++ modifier.modifierType --++ "(" ++ modifierFromQuantity ++ "/" ++ modifierToQuantity ++ ")"
        SimpleRequest amount fromUnit toUnit -> 
            SimpleConversion amount fromUnit toUnit (convert (Measurement amount fromUnit) toUnit)
        WithModifier amount fromUnit modifier toUnit ->
            let
                toValue = convertWithModifier modifier (Measurement amount fromUnit) toUnit
            in
                ConversionWithModifier amount fromUnit modifier toUnit toValue

viewConversionState : Maybe Int -> ConversionState -> Element Message
viewConversionState pinnedIdx state =
    let
        eqView = boxedText [] "="
        pinOrUnpinView = Maybe.map unpinItemView pinnedIdx |> Maybe.withDefault pinItemView
        maybeFromQuantityView = Maybe.map (\_ -> \_ -> dummyEl) pinnedIdx |> Maybe.withDefault fromQuantityView
    in
        case state of
            InvalidConversion msg -> text msg
            SimpleConversion value fromUnit toUnit toAmount ->
                row [spacing 5] 
                [ pinOrUnpinView
                , maybeFromQuantityView fromUnit
                , valueView value fromUnit
                , eqView
                , valueView toAmount toUnit
                ]
            ConversionWithModifier value fromUnit modifier toUnit toValue ->
                row [spacing 5] 
                [ pinOrUnpinView
                , maybeFromQuantityView fromUnit
                , valueView value fromUnit
                , boxedText [] <| modifier.name
                , eqView
                , valueView toValue toUnit
                ]
    
valueView : Float -> Unit -> Element Message
valueView amount unit =
    boxedText [] <| formatNumber amount ++ " " ++ unit.symbol

fromQuantityView : Unit -> Element Message
fromQuantityView unit =
    boxedText [] <| "Convert " ++ unit.quantity

pinItemView : Element Message
pinItemView =
    Input.button 
        []
        { onPress = Just PinItem
        , label = el [Font.color okColor, Font.bold] (text "+")
        }

unpinItemView : Int -> Element Message
unpinItemView idx =
    Input.button 
        []
        { onPress = Just <| UnpinItem idx
        , label = el [Font.color notOkColor, Font.bold] (text "-")
        }

dummyEl : Element Message
dummyEl = el [] (text "")

formatNumber : Float -> String
formatNumber n =
    String.fromFloat n

err : String -> Element Message
err msg =
    text msg

boxedText : List (Attribute Message) -> String -> Element Message
boxedText attr str
    =  el ([ Border.rounded 5, Border.width 1, padding 10, Border.solid ] ++ attr) <| text str

main =
    Browser.sandbox
        { init = { inputText = "", result = Nothing, pinned = [] }
        , update = update
        , view = view
        }

h1 : List (Element.Attribute Message)
h1 = 
    [ Region.heading 1
    , Font.semiBold
    , Font.size 30 
    ]

okColor : Color
okColor = rgb255  0 113 0

notOkColor : Color
notOkColor = rgb255 188 0 0