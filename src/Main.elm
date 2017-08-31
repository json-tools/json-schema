module Main exposing (main)

import Navigation exposing (Location, program, newUrl)
import Html exposing (Html, text, div)
import Html.Attributes as Attrs exposing (style)
import Html.Events exposing (onInput)
import Json.Decode as Decode exposing (decodeString, Value)
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
                    Result.map2 (\v ->
                        setValue v path ( Encode.string str )
                    ) model.value model.schema
                        --|> Debug.log "res"
                        |> Result.withDefault model.value
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
        editMe =
            """
            { "properties": { "passengers": { "items": { "type": "object" } } } }
            """

        res =
            Result.map2
                (\schema val ->
                    div []
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
                Html.text e


form : Value -> Schema -> String -> Html Msg
form val schema subpath =
    let
        path =
            subpath
                |> String.split "/"
                |> List.drop 1
    in
        case implyType val schema subpath of
            Ok (SingleType ObjectType) ->
                getFields val schema subpath
                    |> List.map
                        (\( name, _ ) ->
                            let
                                newSubpath =
                                    subpath ++ "/" ++ name
                            in
                                div []
                                    [ schemataKey newSubpath
                                    , col10 [ form val schema newSubpath ]
                                    ]
                        )
                    |> col10

            Ok (SingleType StringType) ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.string)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    Html.input [ Attrs.value s, onInput <| ValueChange subpath ] []

                                Err e ->
                                    text e
                       )

            Ok (SingleType IntegerType) ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.int)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    Html.input [ Attrs.type_ "number", Attrs.value <| toString s ] []

                                Err e ->
                                    text e
                       )

            Ok (SingleType NumberType) ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.float)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    Html.input [ Attrs.type_ "number", Attrs.value <| toString s ] []

                                Err e ->
                                    text e
                       )

            Ok (SingleType BooleanType) ->
                val
                    |> Decode.decodeValue (Decode.at path Decode.bool)
                    |> (\s ->
                            case s of
                                Ok s ->
                                    Html.input [ Attrs.type_ "checkbox", Attrs.checked s ] []

                                Err e ->
                                    text e
                       )

            Ok (SingleType ArrayType) ->
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
                    |> div [ style [ ( "color", "red" )] ]


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



col10 : List (Html a) -> Html a
col10 =
    div [ style [ ( "padding", "20px" ) ] ]


source : Schema -> Html msg
source s =
    Html.pre [] [ Schema.encode s |> Encode.encode 4 |> text ]


schemataKey : String -> Html msg
schemataKey s =
    Html.code [ style [ ( "font-weight", "bold" ) ] ] [ text s ]


schemataDoc : Maybe Schemata -> String -> String -> Html Msg
schemataDoc s label subpath =
    s
        |> Maybe.map
            (\(Schemata s) ->
                col10
                    [ text label
                    , s
                        |> List.map
                            (\( key, schema ) ->
                                let
                                    newSubpath =
                                        subpath ++ key
                                in
                                    col10
                                        [ schemataKey newSubpath
                                        , documentation schema newSubpath
                                        , source schema
                                        ]
                            )
                        |> div []
                    ]
            )
        |> Maybe.withDefault (text "")


documentation : Schema -> String -> Html Msg
documentation node subpath =
    case node of
        ObjectSchema s ->
            col10
                [ schemataDoc s.definitions "definitions: " <| subpath ++ "/definitions/"
                , schemataDoc s.properties "properties: " <| subpath ++ "/properties/"
                ]

        BooleanSchema b ->
            Html.text <| toString b
