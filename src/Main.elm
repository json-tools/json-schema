module Main exposing (main)

import Navigation exposing (Location, program, newUrl)
import Html exposing (Html)
import StyleSheet exposing (Styles(None, Main, Error, Bordered, Button), Variations, stylesheet)
import Element.Events exposing (onClick, onMouseOver, onMouseOut, onInput)
import Element.Attributes as Attributes exposing (inlineStyle, spacing, padding, alignLeft, height, minWidth, width, yScrollbar, fill, px)
import Element exposing (Element, el, row, text, column)
import Json.Decode as Decode exposing (decodeString, decodeValue, Value)
import Json.Encode as Encode
import Json.Schema.Helpers exposing (implyType, setValue)
import Json.Schema.Examples exposing (coreSchemaDraft6, bookingSchema)
import Json.Schema.Definitions as Schema
    exposing
        ( Schema(BooleanSchema, ObjectSchema)
        , SubSchema
        , Schemata(Schemata)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        , blankSubSchema
        )


type alias View =
    Element Styles Variations Msg


type alias Model =
    { schema : Result String Schema
    , value : Result String Value
    }


type Msg
    = NoOp
    | UrlChange Location
    | ValueChange String String


main : Program Never Model Msg
main =
    program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Location -> ( Model, Cmd Msg )
init location =
    Model
        (coreSchemaDraft6 |> decodeString Schema.decoder)
        (bookingSchema |> decodeString Decode.value)
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ValueChange path str ->
            let
                value =
                    Result.map2
                        (\v ->
                            setValue v path (Encode.string str)
                        )
                        model.value
                        model.schema
                        --|> Debug.log "res"
                        |>
                            Result.withDefault model.value
            in
                { model | value = value } ! []

        _ ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        propertiesListing section fn =
            model.value
                |> Result.andThen (decodeValue Schema.decoder)
                |> Result.toMaybe
                |> Maybe.andThen
                    (\s ->
                        case s of
                            ObjectSchema os ->
                                Just os

                            _ ->
                                Nothing
                    )
                |> Maybe.andThen fn
                |> Maybe.withDefault (Schemata [])
                |> (\(Schemata props) ->
                        List.map (\( key, _ ) -> row None [] [ Element.link ("#/" ++ section ++ "/" ++ key) <| text key ]) props
                   )
                |> column None [ padding 20 ]

        a =
            model.value
                |> Result.andThen (decodeValue Schema.decoder)
                |> Result.map (documentation "#")
                |> Result.withDefault (text "")
    in
        Element.viewport stylesheet <|
            row Main
                [ height <| fill 1
                , width <| fill 1
                ]
                [ column None
                    [ height <| fill 1, minWidth <| px 200, yScrollbar, spacing 10, padding 0 ]
                    [ el None [ inlineStyle [ ( "font-weight", "bold" ) ] ] <| text "definitions"
                    , propertiesListing "definitions" .definitions
                    , el None [ inlineStyle [ ( "font-weight", "bold" ) ] ] <| text "properties"
                    , propertiesListing "properties" .properties
                    ]
                , column None
                    [ height <| fill 1, width <| fill 1, yScrollbar, padding 10 ]
                    [ a
                    ]
                ]


viewForm : Model -> View
viewForm model =
    let
        editMe =
            """
            { "properties": { "passengers": { "items": { "type": "object" } } } }
            """

        res =
            Result.map2
                (\schema val ->
                    column None
                        []
                        --[ schema |> toString |> text
                        [ form val schema "#"
                        ]
                )
                model.schema
                model.value
    in
        case res of
            Ok html ->
                html

            Err e ->
                text e


form : Value -> Schema -> String -> View
form val schema subpath =
    let
        path =
            subpath
                |> String.split "/"
                |> List.drop 1

        implied =
            implyType val schema subpath
    in
        case implied.type_ of
            SingleType ObjectType ->
                getFields val schema subpath
                    |> List.map
                        (\( name, _ ) ->
                            let
                                newSubpath =
                                    subpath ++ "/" ++ name
                            in
                                column None
                                    []
                                    [ schemataKey subpath newSubpath
                                    , col10 [ form val schema newSubpath ]
                                    ]
                        )
                    |> col10

            SingleType StringType ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.string)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    case implied.schema.enum of
                                        Nothing ->
                                            Element.inputText None [ onInput <| ValueChange subpath ] s

                                        Just enum ->
                                            enum
                                                |> List.map
                                                    (\v ->
                                                        v
                                                            |> Decode.decodeValue Decode.string
                                                            |> Result.withDefault ""
                                                            |> (\val ->
                                                                    Element.option val (s == val) (text val)
                                                               )
                                                    )
                                                |> Element.select ""
                                                    None
                                                    [ onInput <| ValueChange subpath ]

                                Err e ->
                                    text e
                       )

            SingleType IntegerType ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.int)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    Element.inputText None [ Attributes.type_ "number" ] (toString s)

                                Err e ->
                                    text e
                       )

            SingleType NumberType ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.float)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    Element.inputText None [ Attributes.type_ "number" ] (toString s)

                                Err e ->
                                    text e
                       )

            SingleType BooleanType ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.bool)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    Element.checkbox s None [ Attributes.type_ "checkbox" ] (text "")

                                Err e ->
                                    text e
                       )

            SingleType ArrayType ->
                val
                    |> Decode.decodeValue (Decode.at path (Decode.list Decode.value))
                    |> (\list ->
                            case list of
                                Ok list ->
                                    text <| toString list

                                Err e ->
                                    text e
                       )

            x ->
                x
                    |> toString
                    |> (++) "some other type detected: "
                    |> text
                    |> (\s -> [ s ])
                    |> column Error []


getFields : Value -> Schema -> String -> List ( String, Value )
getFields val schema subpath =
    let
        path =
            subpath
                |> String.split "/"
                |> List.drop 1
    in
        val
            |> Decode.decodeValue (Decode.at path <| Decode.keyValuePairs Decode.value)
            |> Result.withDefault []
            |> List.reverse


col10 : List View -> View
col10 =
    column None [ spacing 10, padding 10 ]


source : Schema -> View
source s =
    Element.node "pre" <| el None [] (Schema.encode s |> Encode.encode 4 |> text)


schemataKey : String -> String -> View
schemataKey parent s =
    let
        nodeName =
            if parent == "#/definitions/" || parent == "#/properties/" then
                "h1"
            else
                "h3"
    in
        Element.node nodeName <|
            el None
                [ Attributes.tabindex 1
                , Attributes.id <| String.dropLeft 1 s
                , inlineStyle [ ( "font-weight", "bold" ) ]
                ]
                (text s)


schemataDoc : Maybe Schemata -> String -> String -> View
schemataDoc s label subpath =
    s
        |> Maybe.map
            (\(Schemata s) ->
                col10
                    [ text ""
                    , s
                        |> List.map
                            (\( key, schema ) ->
                                let
                                    newSubpath =
                                        subpath ++ key
                                in
                                    col10
                                        [ schemataKey subpath newSubpath
                                        , documentation newSubpath schema
                                        , source schema
                                        ]
                            )
                        |> column None []
                    ]
            )
        |> Maybe.withDefault (text "")


documentation : String -> Schema -> View
documentation subpath node =
    case node of
        ObjectSchema s ->
            col10
                [ schemataDoc s.definitions "definitions: " <| subpath ++ "/definitions/"
                , schemataDoc s.properties "properties: " <| subpath ++ "/properties/"
                ]

        BooleanSchema b ->
            text <| toString b
