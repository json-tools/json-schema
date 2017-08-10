module Flat exposing (all)

import Test exposing (Test, describe, test, only)
import Json.Schema.Builder exposing (
    blankSchema, withType, withNullableType, withUnionType )
import Data.Schema exposing (Schema, decoder)
import Expect
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (decodeValue)


all : Test
all =
    describe "schema.type"
        [ test "type=integer" <|
            \() ->
                [ ( "type", Encode.string "integer" ) ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (blankSchema
                            |> withType "integer"
                        )
        , test "type=number" <|
            \() ->
                [ ( "type", Encode.string "number" ) ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (blankSchema
                            |> withType "number"
                        )
        , test "type=string" <|
            \() ->
                [ ( "type", Encode.string "string" ) ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (blankSchema
                            |> withType "string"
                        )
        , test "type=object" <|
            \() ->
                [ ( "type", Encode.string "object" ) ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (blankSchema
                            |> withType "object"
                        )
        , test "type=array" <|
            \() ->
                [ ( "type", Encode.string "array" ) ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (blankSchema
                            |> withType "array"
                        )
        , test "type=null" <|
            \() ->
                [ ( "type", Encode.string "null" ) ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (blankSchema
                            |> withType "null"
                        )
        , test "type=[null,integer]" <|
            \() ->
                [ ( "type", Encode.list
                    [ Encode.string "null"
                    , Encode.string "integer"
                    ]
                ) ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (blankSchema
                            |> withNullableType "integer"
                        )
        , test "type=[string,integer]" <|
            \() ->
                [ ( "type", Encode.list
                    [ Encode.string "string"
                    , Encode.string "integer"
                    ]
                ) ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (blankSchema
                            |> withUnionType [ "string", "integer" ]
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
        |> decodeValue decoder
