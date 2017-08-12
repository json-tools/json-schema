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
    , describe "exclusiveMaximum"
        [ test "success" <|
            \() ->
                { blankSchema | exclusiveMaximum = Just 2 }
                    |> validate (Encode.float 1.9)
                    |> Expect.equal (Ok True)
        , test "failure" <|
            \() ->
                { blankSchema | exclusiveMaximum = Just 2 }
                    |> validate (Encode.float 2)
                    |> Expect.equal (Err "Value is not below the exclusive maximum of 2")
        ]
    , describe "exclusiveMinimum"
        [ test "success" <|
            \() ->
                { blankSchema | exclusiveMinimum = Just 2 }
                    |> validate (Encode.float 2.1)
                    |> Expect.equal (Ok True)
        , test "failure" <|
            \() ->
                { blankSchema | exclusiveMinimum = Just 2 }
                    |> validate (Encode.float 2)
                    |> Expect.equal (Err "Value is not above the exclusive minimum of 2")
        ]
    , describe "maxLength"
        [ test "success" <|
            \() ->
                { blankSchema | maxLength = Just 3 }
                    |> validate (Encode.string "foo")
                    |> Expect.equal (Ok True)
        , test "failure" <|
            \() ->
                { blankSchema | maxLength = Just 2 }
                    |> validate (Encode.string "foo")
                    |> Expect.equal (Err "String is longer than expected 2")
        ]
    , describe "minLength"
        [ test "success" <|
            \() ->
                { blankSchema | minLength = Just 3 }
                    |> validate (Encode.string "foo")
                    |> Expect.equal (Ok True)
        , test "failure" <|
            \() ->
                { blankSchema | minLength = Just 4 }
                    |> validate (Encode.string "foo")
                    |> Expect.equal (Err "String is shorter than expected 4")
        ]
    , describe "pattern"
        [ test "success" <|
            \() ->
                { blankSchema | pattern = Just "o{2}" }
                    |> validate (Encode.string "foo")
                    |> Expect.equal (Ok True)
        , test "failure" <|
            \() ->
                { blankSchema | pattern = Just "o{3}" }
                    |> validate (Encode.string "foo")
                    |> Expect.equal (Err "String does not match the regex pattern")
        ]
    ]
