module Validation exposing (all)

--import Json.Schema.Builder exposing (buildSchema)
import Data.Schema exposing (blankSchema)
import Json.Schema exposing (validate)
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Expect

all : Test
all = describe "validations"
    [ describe "multipleOf"
        [ test "success with int" <|
            \() ->
                { blankSchema | multipleOf = Just 2 }
                    |> validate (Encode.int 4)
                    |> Expect.equal (Ok True)
        , test "success with float" <|
            \() ->
                { blankSchema | multipleOf = Just 2.1 }
                    |> validate (Encode.float 4.2)
                    |> Expect.equal (Ok True)
        , test "success with periodic float" <|
            \() ->
                { blankSchema | multipleOf = Just (1/3) }
                    |> validate (Encode.float (2/3))
                    |> Expect.equal (Ok True)
        , test "failure" <|
            \() ->
                { blankSchema | multipleOf = Just 3 }
                    |> validate (Encode.float (2/7))
                    |> Expect.equal (Err "Value is not the multiple of 3")
        ]
    , describe "maximum"
        [ test "success" <|
            \() ->
                { blankSchema | maximum = Just 2 }
                    |> validate (Encode.int 2)
                    |> Expect.equal (Ok True)
        , test "failure" <|
            \() ->
                { blankSchema | maximum = Just 2 }
                    |> validate (Encode.float 2.1)
                    |> Expect.equal (Err "Value is above the maximum of 2")
        ]
    , describe "minimum"
        [ test "success" <|
            \() ->
                { blankSchema | minimum = Just 2 }
                    |> validate (Encode.int 2)
                    |> Expect.equal (Ok True)
        , test "failure" <|
            \() ->
                { blankSchema | minimum = Just 2 }
                    |> validate (Encode.float 1.9)
                    |> Expect.equal (Err "Value is below the minimum of 2")
        ]
    ]
