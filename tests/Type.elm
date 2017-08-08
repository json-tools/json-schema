module Type exposing (all)

import Json.Schema as JS exposing (empty)
import Test exposing (Test, describe, test)
import Json.Schema.Definitions exposing (Schema(IntegerSchema, Undefined), IntSchema)
import Expect
import Json.Encode as Encode exposing (Value)


all : Test
all =
    describe "schema.type"
        [ test "integer schema" <|
            \() ->
                [ ( "type", Encode.string "integer" ) ]
                    |> decodeSchema
                    |> shouldResultWithBlankIntSchema
        ]


shouldResultWithBlankIntSchema : Result x Schema -> Expect.Expectation
shouldResultWithBlankIntSchema =
    IntSchema Nothing Nothing
        |> IntegerSchema
        |> Ok
        |> Expect.equal


decodeSchema : List (String, Value) -> Result String Schema
decodeSchema list =
    list
        |> Encode.object
        |> JS.fromValue
