port module Main exposing (main)

import Navigation exposing (Location, program, newUrl)
import Html exposing (Html)
import Html.Attributes
import Dom
import Task
import StyleSheet exposing (Styles(None, Main, Error, SchemaHeader, JsonEditor, MenuItem, NoOutline), Variations(Active), stylesheet)
import Element.Events exposing (on, onClick, onMouseOver, onMouseOut, onInput, onCheck)
import Element.Attributes as Attributes exposing (vary, inlineStyle, spacing, padding, alignLeft, height, minWidth, maxWidth, width, yScrollbar, fill, px, percent)
import Element exposing (Element, el, row, text, column, paragraph)
import Markdown
import Json.Decode as Decode exposing (decodeString, decodeValue, Value)
import Json.Encode as Encode
import Json.Schema.Helpers exposing (implyType, setValue, for)
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
    , activeSection : String
    , error : Maybe String
    }


type Msg
    = NoOp
    | UrlChange Location
    | StringChange String String
    | NumberChange String String
    | BooleanChange String Bool
    | ValueChange String String
    | ContentScroll
    | ActiveSection String


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
        location.hash -- activeSection
        Nothing -- error
        ! [ location.hash |> String.dropLeft 1 |> Dom.focus |> Task.attempt (\_ -> NoOp) ]


updateValue : Model -> String -> Result String Value -> Model
updateValue model path newStuff =
    case newStuff of
        Ok val ->
            model.schema
                |> Result.map2 (\v -> setValue v path val) model.value
                -- |> Debug.log "res"
                |> Result.withDefault model.value
                |> (\v -> { model | value = v, error = Nothing })

        Err s ->
            { model | error = Just s }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StringChange path str ->
            updateValue model path (str |> Encode.string |> Ok) ! []

        NumberChange path str ->
            updateValue model path (str |> String.toFloat |> Result.map Encode.float) ! []

        BooleanChange path bool ->
            updateValue model path (bool |> Encode.bool |> Ok) ! []

        ValueChange path str ->
            updateValue model path (str |> decodeString Decode.value) ! []

        UrlChange l ->
            { model | activeSection = l.hash } ! []

        ActiveSection s ->
            { model | activeSection = s } ! []

        _ ->
            model ! []


port activeSection : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    activeSection ActiveSection


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
                        List.map
                            (\( key, _ ) ->
                                let
                                    thisSection =
                                        "#/" ++ section ++ "/" ++ key
                                in
                                    row None
                                        []
                                        [ key
                                            |> text
                                            |> el MenuItem
                                                [ vary Active <| thisSection == model.activeSection
                                                , padding 5
                                                , width <| fill 1
                                                ]
                                            |> Element.link thisSection
                                        ]
                            )
                            props
                   )
                |> column None [ padding 20 ]

        a =
            model.value
                |> Result.andThen (decodeValue Schema.decoder)
                |> Result.map2 (\metaSchema schema -> documentation 0 "#" schema metaSchema) model.schema
                |> Result.withDefault (text "")
    in
        Element.viewport stylesheet <|
            row Main
                [ height <| fill 1
                , width <| fill 1
                ]
                [ column None
                    [ height <| fill 1, minWidth <| px 200, yScrollbar, spacing 10, padding 0 ]
                    [ Element.bold "definitions"
                    , propertiesListing "definitions" .definitions
                    , Element.bold "properties"
                    , propertiesListing "properties" .properties
                    ]
                , column None
                    [ height <| fill 1
                    , width <| fill 1
                    , yScrollbar
                    , padding 10
                    , Attributes.id "content"
                      -- , on "scroll" (Json.Decode.succeed ContentScroll)
                    ]
                    [ case model.error of
                        Just s ->
                            text s
                        Nothing ->
                            text ""
                    , a
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
                        [ form val schema "#" ""
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


form : Value -> Schema -> String -> String -> View
form val schema subpath key =
    let
        path =
            subpath
                |> String.split "/"
                |> List.drop 1
                |> (\s -> s ++ [ key ])

        jsonPointer =
            path
                |> String.join "/"
                |> (\x -> "#/" ++ x)

        implied =
            implyType val schema subpath
    in
        case implied.type_ of
            SingleType StringType ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.string)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    case implied.schema.enum of
                                        Nothing ->
                                            Element.inputText None [ onInput <| StringChange jsonPointer ] s

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
                                                    [ onInput <| StringChange subpath ]

                                Err e ->
                                    text e
                       )

            SingleType IntegerType ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.int)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    Element.inputText None [ Attributes.type_ "number", onInput <| NumberChange jsonPointer ] (toString s)

                                Err e ->
                                    text e
                       )

            SingleType NumberType ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.float)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    Element.inputText None [ Attributes.type_ "number", onInput <| NumberChange jsonPointer ] (toString s)

                                Err e ->
                                    text e
                       )

            SingleType BooleanType ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.bool)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    Element.checkbox s None [ Attributes.type_ "checkbox", onCheck <| BooleanChange jsonPointer ] (text "")

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

            SingleType ObjectType ->
                let
                    editAsValue =
                        True
                in
                    if editAsValue then
                        val
                            |> Encode.encode 4
                            |> Element.textArea JsonEditor [ Attributes.rows 10, onInput <| ValueChange jsonPointer ]
                    else
                        getFields val schema subpath
                            |> List.map
                                (\( name, _ ) ->
                                    let
                                        newSubpath =
                                            subpath ++ "/" ++ key ++ "/" ++ name
                                    in
                                            column None
                                                []
                                                [ schemataKey 0 subpath name
                                                , col10 [ form val schema subpath name ]
                                                ]
                                )
                            |> col10

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
    Markdown.toHtml [ Html.Attributes.class "hljs" ] (Schema.encode s |> Encode.encode 2 |> (\s -> "```json\n" ++ s ++ "```"))
        |> Element.html
        |> el None []


schemataKey : Int -> String -> String -> View
schemataKey level parent s =
    let
        nodeName =
            if level == 0 then
                "h1"
            else
                "h2"
    in
        Element.node nodeName <|
            el SchemaHeader [] (text s)


schemataDoc : Int -> Maybe Schemata -> Schema -> String -> View
schemataDoc level s metaSchema subpath =
    let
        takeHalfWidth =
            el None [ width <| percent 50 ]

        dropAnchor newSubpath =
            el NoOutline
                [ Attributes.tabindex 1
                , Attributes.id <| String.dropLeft 1 newSubpath
                ]

        displayNextToEachOther list =
            row None
                [ Attributes.justify ]
                (list |> List.map takeHalfWidth)

        displayOneUnderAnother list =
            column None [] list

        printProperty (key, schema) =
            let
                newSubpath =
                    subpath ++ key
            in
                case metaSchema |> for subpath of
                    Just ms ->
                        if level == 0 then
                            [ key
                                |> schemataKey level subpath
                                |> dropAnchor newSubpath
                            , displayNextToEachOther
                                [ documentation (level + 1) newSubpath schema ms
                                , source schema
                                ]
                            ]
                        else
                            [ key
                                |> schemataKey level subpath
                            , displayOneUnderAnother
                                [ documentation (level + 1) newSubpath schema ms
                                , source schema
                                ]
                            ]

                    Nothing ->
                        []

        printSchemata (Schemata s) =
            col10
                [ s
                    |> List.map (printProperty >> col10)
                    |> column None []
                ]
    in
        s
            |> Maybe.map printSchemata
            |> Maybe.withDefault (text "")


metaDoc : SubSchema -> View
metaDoc s =
    column None
        []
        [ row None [ inlineStyle [ ("font-size", "18px") ] ] [ s.title |> Maybe.withDefault "" |> Element.bold ]
        , paragraph None [ inlineStyle [ ("font-size", "16px") ] ] [ s.description |> Maybe.withDefault "" |> Markdown.toHtml [] |> Element.html ]
        ]


documentation : Int -> String -> Schema -> Schema -> View
documentation level jsonPointer schema metaSchema =
    case schema of
        ObjectSchema s ->
            col10
                [ metaDoc s
                , schemataDoc level s.definitions metaSchema <| jsonPointer ++ "/definitions/"
                , schemataDoc level s.properties metaSchema <| jsonPointer ++ "/properties/"
                , if s.properties == Nothing && s.definitions == Nothing then
                    form (schema |> Schema.encode) metaSchema jsonPointer ""
                  else
                    text ""
                ]

        BooleanSchema b ->
            text <| toString b
