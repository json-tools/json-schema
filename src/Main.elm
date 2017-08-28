module Main exposing (main)

import Navigation exposing (Location, program, newUrl)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (decodeString)
import Json.Schema.Helpers as Helpers exposing (typeToString)
import Json.Schema.Definitions as Schema exposing
    ( Schema(BooleanSchema, ObjectSchema)
    , Schemata(Schemata)
    )

type alias Model = {}


type Msg
    = NoOp
    | UrlChange Location


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
    {} ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case coreSchemaDraft6 |> decodeString Schema.decoder of
        Ok s ->
            div []
                [ Html.pre [] [ Html.text <| toString s ]
                , documentation s
                ]

        Err e ->
            Html.text e

col10 : List (Html a) -> Html a
col10 =
    div [ style [ ("padding", "10px") ] ]


schemata : Maybe Schemata -> (List (String, Schema) -> Html msg) -> Html msg
schemata s fn =
    s
        |> Maybe.map (\(Schemata s) -> fn s)
        |> Maybe.withDefault (text "")


documentation : Schema -> Html Msg
documentation node =
    case node of
        ObjectSchema s ->
            col10
                [ schemata s.definitions (\defs ->
                    col10
                        [ text "definitions: "
                        , defs
                            |> List.map (\(key, schema) ->
                                col10
                                    [ Html.code [] [ text <| "#/definitions/" ++ key ]
                                    , text " => "
                                    , documentation schema
                                    ]
                            )
                            |> div []
                        ]
                    )
                , Html.text (typeToString s.type_)
                , schemata s.properties (\props ->
                    col10
                        [ text "properties: "
                        , props
                            |> List.map (\(key, schema) ->
                                col10
                                    [ Html.code [] [ text <| "#/properties/" ++ key ]
                                    , text " => "
                                    , documentation schema
                                    ]
                            )
                            |> div []
                        ]
                    )
                ]

        BooleanSchema b ->
            Html.text (toString b)


coreSchemaDraft6 : String
coreSchemaDraft6 = """
{
    "$schema": "http://json-schema.org/draft-06/schema#",
    "$id": "http://json-schema.org/draft-06/schema#",
    "title": "Core schema meta-schema",
    "definitions": {
        "schemaArray": {
            "type": "array",
            "minItems": 1,
            "items": { "$ref": "#" }
        },
        "nonNegativeInteger": {
            "type": "integer",
            "minimum": 0
        },
        "nonNegativeIntegerDefault0": {
            "allOf": [
                { "$ref": "#/definitions/nonNegativeInteger" },
                { "default": 0 }
            ]
        },
        "simpleTypes": {
            "enum": [
                "array",
                "boolean",
                "integer",
                "null",
                "number",
                "object",
                "string"
            ]
        },
        "stringArray": {
            "type": "array",
            "items": { "type": "string" },
            "uniqueItems": true,
            "default": []
        }
    },
    "type": ["object", "boolean"],
    "properties": {
        "$id": {
            "type": "string",
            "format": "uri-reference"
        },
        "$schema": {
            "type": "string",
            "format": "uri"
        },
        "$ref": {
            "type": "string",
            "format": "uri-reference"
        },
        "title": {
            "type": "string"
        },
        "description": {
            "type": "string"
        },
        "default": {},
        "multipleOf": {
            "type": "number",
            "exclusiveMinimum": 0
        },
        "maximum": {
            "type": "number"
        },
        "exclusiveMaximum": {
            "type": "number"
        },
        "minimum": {
            "type": "number"
        },
        "exclusiveMinimum": {
            "type": "number"
        },
        "maxLength": { "$ref": "#/definitions/nonNegativeInteger" },
        "minLength": { "$ref": "#/definitions/nonNegativeIntegerDefault0" },
        "pattern": {
            "type": "string",
            "format": "regex"
        },
        "additionalItems": { "$ref": "#" },
        "items": {
            "anyOf": [
                { "$ref": "#" },
                { "$ref": "#/definitions/schemaArray" }
            ],
            "default": {}
        },
        "maxItems": { "$ref": "#/definitions/nonNegativeInteger" },
        "minItems": { "$ref": "#/definitions/nonNegativeIntegerDefault0" },
        "uniqueItems": {
            "type": "boolean",
            "default": false
        },
        "contains": { "$ref": "#" },
        "maxProperties": { "$ref": "#/definitions/nonNegativeInteger" },
        "minProperties": { "$ref": "#/definitions/nonNegativeIntegerDefault0" },
        "required": { "$ref": "#/definitions/stringArray" },
        "additionalProperties": { "$ref": "#" },
        "definitions": {
            "type": "object",
            "additionalProperties": { "$ref": "#" },
            "default": {}
        },
        "properties": {
            "type": "object",
            "additionalProperties": { "$ref": "#" },
            "default": {}
        },
        "patternProperties": {
            "type": "object",
            "additionalProperties": { "$ref": "#" },
            "default": {}
        },
        "dependencies": {
            "type": "object",
            "additionalProperties": {
                "anyOf": [
                    { "$ref": "#" },
                    { "$ref": "#/definitions/stringArray" }
                ]
            }
        },
        "propertyNames": { "$ref": "#" },
        "const": {},
        "enum": {
            "type": "array",
            "minItems": 1,
            "uniqueItems": true
        },
        "type": {
            "anyOf": [
                { "$ref": "#/definitions/simpleTypes" },
                {
                    "type": "array",
                    "items": { "$ref": "#/definitions/simpleTypes" },
                    "minItems": 1,
                    "uniqueItems": true
                }
            ]
        },
        "format": { "type": "string" },
        "allOf": { "$ref": "#/definitions/schemaArray" },
        "anyOf": { "$ref": "#/definitions/schemaArray" },
        "oneOf": { "$ref": "#/definitions/schemaArray" },
        "not": { "$ref": "#" }
    },
    "default": {}
}
"""
