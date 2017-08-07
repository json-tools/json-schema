module Type exposing (all)

import Json.Schema as JS exposing (Schema, empty)
import Json.Encode as Encode exposing (Value, string, int, object, list)
import Test exposing (Test, describe, test)
import Expect


all : Test
all =
    describe "Json Schema"
        [ describe "Creation"
            [ test "from a valid json string" <|
                \() ->
                    JS.fromString "{}"
                        |> Expect.equal (Ok empty)
            ]
        ]
