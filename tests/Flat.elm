module Flat exposing (all)

import Test exposing (Test, describe, test, only)
import Json.Schema.Builder
    exposing
        ( buildSchema
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
import Data.Schema exposing (Schema, decoder, blankSchema)
import Expect
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (decodeValue)


all : Test
all =
    describe "schema flat parser"
        [ test "type=integer" <|
            \() ->
                [ ( "type", Encode.string "integer" ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (buildSchema
                            |> withType "integer"
                            |> toSchema
                        )
        , test "type=number" <|
            \() ->
                [ ( "type", Encode.string "number" ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (buildSchema
                            |> withType "number"
                            |> toSchema
                        )
        , test "type=string" <|
            \() ->
                [ ( "type", Encode.string "string" ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (buildSchema
                            |> withType "string"
                            |> toSchema
                        )
        , test "type=object" <|
            \() ->
                [ ( "type", Encode.string "object" ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (buildSchema
                            |> withType "object"
                            |> toSchema
                        )
        , test "type=array" <|
            \() ->
                [ ( "type", Encode.string "array" ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (buildSchema
                            |> withType "array"
                            |> toSchema
                        )
        , test "type=null" <|
            \() ->
                [ ( "type", Encode.string "null" ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (buildSchema
                            |> withType "null"
                            |> toSchema
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
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema
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
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema
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
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema |> withDefinitions [ ( "foo", blankSchema ) ])
        , test "items=[blankSchema]" <|
            \() ->
                [ ( "items", Encode.list <| [ Encode.object [] ] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema |> withItems [ blankSchema ])
        , test "items=blankSchema" <|
            \() ->
                [ ( "items", Encode.object [] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        ( blankSchema |> withItem blankSchema )
        , test "additionalItems=blankSchema" <|
            \() ->
                [ ( "additionalItems", Encode.object [] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        ( blankSchema |> withAdditionalItems blankSchema )
        , test "contains={}" <|
            \() ->
                [ ( "contains", Encode.object [] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema |> withContains blankSchema)
        , test "properties={foo=blankSchema}" <|
            \() ->
                [ ( "properties", Encode.object [ ( "foo", Encode.object [] ) ] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema |> withProperties [ ( "foo", blankSchema ) ])
        , test "patternProperties={foo=blankSchema}" <|
            \() ->
                [ ( "patternProperties", Encode.object [ ( "foo", Encode.object [] ) ] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema |> withPatternProperties [ ( "foo", blankSchema ) ])
        , test "additionalProperties=blankSchema" <|
            \() ->
                [ ( "additionalProperties", Encode.object [] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        ( blankSchema |> withAdditionalProperties blankSchema )
        , test "dependencies={foo=blankSchema}" <|
            \() ->
                [ ( "dependencies", Encode.object [ ( "foo", Encode.object [] ) ] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema |> withSchemaDependency "foo" blankSchema)
        , test "dependencies={foo=[bar]}" <|
            \() ->
                [ ( "dependencies", Encode.object [ ( "foo", Encode.list [ Encode.string "bar" ] ) ] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema |> withPropNamesDependency "foo" [ "bar" ])
        , test "propertyNames={}" <|
            \() ->
                [ ( "propertyNames", Encode.object [ ( "type", Encode.string "string" ) ] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema |> withPropertyNames stringSchema)
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
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema |> withAllOf [ blankSchema ])
        , test "oneOf=[blankSchema]" <|
            \() ->
                [ ( "oneOf", Encode.list [ Encode.object [] ] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema |> withOneOf [ blankSchema ])
        , test "anyOf=[blankSchema]" <|
            \() ->
                [ ( "anyOf", Encode.list [ Encode.object [] ] ) ]
                    |> decodeSchema
                    |> Expect.equal
                        (blankSchema |> withAnyOf [ blankSchema ])
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
        |> decodeValue decoder


stringSchema : Schema
stringSchema =
    buildSchema
        |> withType "string"
        |> toSchema
        |> Result.withDefault blankSchema
