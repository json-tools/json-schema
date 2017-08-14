module Flat exposing (all)

import Test exposing (Test, describe, test, only)
import Json.Schema.Builder
    exposing
        ( SchemaBuilder
        , buildSchema
        , toSchema
        , withType
        , withNullableType
        , withUnionType
        , withContains
        , withDefinitions
        , withItems
        , withItem
        , withAdditionalItems
        , withProperties
        , withPatternProperties
        , withAdditionalProperties
        , withSchemaDependency
        , withPropNamesDependency
        , withPropertyNames
        , withAllOf
        , withAnyOf
        , withOneOf
        )
import Data.Schema as Schema exposing (Schema, decoder, blankSchema)
import Expect
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (decodeValue)


all : Test
all =
    describe "schema flat parser"
        [ test "type=integer" <|
            \() ->
                [ ( "type", Encode.string "integer" ) ]
                    |> decodesInto
                        (buildSchema
                            |> withType "integer"
                        )
        , test "type=number" <|
            \() ->
                [ ( "type", Encode.string "number" ) ]
                    |> decodesInto
                        (buildSchema
                            |> withType "number"
                        )
        , test "type=string" <|
            \() ->
                [ ( "type", Encode.string "string" ) ]
                    |> decodesInto
                        (buildSchema
                            |> withType "string"
                        )
        , test "type=object" <|
            \() ->
                [ ( "type", Encode.string "object" ) ]
                    |> decodesInto
                        (buildSchema
                            |> withType "object"
                        )
        , test "type=array" <|
            \() ->
                [ ( "type", Encode.string "array" ) ]
                    |> decodesInto
                        (buildSchema
                            |> withType "array"
                        )
        , test "type=null" <|
            \() ->
                [ ( "type", Encode.string "null" ) ]
                    |> decodesInto
                        (buildSchema
                            |> withType "null"
                        )
        , test "type=[null,integer]" <|
            \() ->
                [ ( "type"
                  , Encode.list
                        [ Encode.string "null"
                        , Encode.string "integer"
                        ]
                  )
                ]
                    |> decodesInto
                        (buildSchema
                            |> withNullableType "integer"
                        )
        , test "type=[string,integer]" <|
            \() ->
                [ ( "type"
                  , Encode.list
                        [ Encode.string "string"
                        , Encode.string "integer"
                        ]
                  )
                ]
                    |> decodesInto
                        (buildSchema
                            |> withUnionType [ "string", "integer" ]
                        )
        , test "title=smth" <|
            \() ->
                [ ( "title", Encode.string "smth" ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (Ok { blankSchema | title = Just "smth" })
        , test "definitions={foo=blankSchema}" <|
            \() ->
                [ ( "definitions", Encode.object [ ( "foo", Encode.object [] ) ] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withDefinitions [ ( "foo", blankSchema ) ]
                        )
        , test "items=[blankSchema]" <|
            \() ->
                [ ( "items", Encode.list <| [ Encode.object [] ] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withItems [ blankSchema ]
                        )
        , test "items=blankSchema" <|
            \() ->
                [ ( "items", Encode.object [] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withItem blankSchema
                        )
        , test "additionalItems=blankSchema" <|
            \() ->
                [ ( "additionalItems", Encode.object [] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withAdditionalItems blankSchema
                        )
        , test "contains={}" <|
            \() ->
                [ ( "contains", Encode.object [] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withContains buildSchema
                        )
        , test "properties={foo=blankSchema}" <|
            \() ->
                [ ( "properties", Encode.object [ ( "foo", Encode.object [] ) ] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withProperties [ ( "foo", blankSchema ) ]
                        )
        , test "patternProperties={foo=blankSchema}" <|
            \() ->
                [ ( "patternProperties", Encode.object [ ( "foo", Encode.object [] ) ] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withPatternProperties [ ( "foo", blankSchema ) ]
                        )
        , test "additionalProperties=blankSchema" <|
            \() ->
                [ ( "additionalProperties", Encode.object [] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withAdditionalProperties blankSchema
                        )
        , test "dependencies={foo=blankSchema}" <|
            \() ->
                [ ( "dependencies", Encode.object [ ( "foo", Encode.object [] ) ] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withSchemaDependency "foo" blankSchema
                        )
        , test "dependencies={foo=[bar]}" <|
            \() ->
                [ ( "dependencies", Encode.object [ ( "foo", Encode.list [ Encode.string "bar" ] ) ] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withPropNamesDependency "foo" [ "bar" ]
                        )
        , test "propertyNames={}" <|
            \() ->
                [ ( "propertyNames", Encode.object [ ( "type", Encode.string "string" ) ] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withPropertyNames stringSchema
                        )
        , test "enum=[]" <|
            \() ->
                [ ( "enum", Encode.list [] ) ]
                    |> decodeSchema
                    |> Expect.err
        , test "allOf=[]" <|
            \() ->
                [ ( "allOf", Encode.list [] ) ]
                    |> decodeSchema
                    |> Expect.err
        , test "allOf=[blankSchema]" <|
            \() ->
                [ ( "allOf", Encode.list [ Encode.object [] ] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withAllOf [ blankSchema ]
                        )
        , test "oneOf=[blankSchema]" <|
            \() ->
                [ ( "oneOf", Encode.list [ Encode.object [] ] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withOneOf [ blankSchema ]
                        )
        , test "anyOf=[blankSchema]" <|
            \() ->
                [ ( "anyOf", Encode.list [ Encode.object [] ] ) ]
                    |> decodesInto
                        (buildSchema
                            |> withAnyOf [ blankSchema ]
                        )
        ]


shouldResultWithSchema : Schema -> Result x Schema -> Expect.Expectation
shouldResultWithSchema s =
    s
        |> Ok
        |> Expect.equal


decodeSchema : List ( String, Value ) -> Result String Schema
decodeSchema list =
    list
        |> Encode.object
        |> decodeValue Schema.decoder


decodesInto : SchemaBuilder -> List ( String, Value ) -> Expect.Expectation
decodesInto sb list =
    list
        |> Encode.object
        |> decodeValue Schema.decoder
        |> Expect.equal (sb |> toSchema)


stringSchema : Schema
stringSchema =
    buildSchema
        |> withType "string"
        |> toSchema
        |> Result.withDefault blankSchema
