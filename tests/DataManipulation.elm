module DataManipulation exposing (all)

import Test exposing (Test, test, describe)
import Expect
import Json.Encode as Encode
import Json.Schema.Helpers as Helpers exposing (setValue)
import Json.Schema.Builder exposing (toSchema, buildSchema, withType, withProperties)

all : Test
all =
    describe "data manipulation"
        [ describe "setValue"
            [ test "simple string value" <|
                \() ->
                    buildSchema
                        |> withType "string"
                        |> toSchema
                        |> Result.andThen (setValue Encode.null "#/" (Encode.string "test"))
                        |> Expect.equal (Ok <| Encode.string "test")
            , test "string property in object" <|
                \() ->
                    buildSchema
                        |> withProperties
                            [ ( "foo", buildSchema |> withType "string" )
                            ]
                        |> toSchema
                        |> Result.andThen (setValue Encode.null "#/" (Encode.string "test"))
                        |> Expect.equal (Ok <| Encode.object [ ( "foo", Encode.string "test" ) ])
            ]
        ]
