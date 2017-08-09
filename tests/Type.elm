module Type exposing (all)

import Json.Schema as JS exposing (empty)
import Test exposing (Test, describe, test, only)
import Data.Schema
    exposing
        ( Schema
            ( IntegerSchema
            , FloatSchema
            , StringSchema
            , Undefined
            )
        , Meta
        )
import Data.NumberValidations exposing (NumberValidations)
import Data.StringValidations exposing (StringValidations)
import Expect
import Json.Encode as Encode exposing (Value)


all : Test
all =
    describe "schema.type"
        [ test "integer schema" <|
            \() ->
                [ ( "type", Encode.string "integer" ) ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (IntegerSchema
                            withEmptyMeta
                            (NumberValidations
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                            )
                        )
        , test "integer schema with validations" <|
            \() ->
                [ ( "type", Encode.string "integer" )
                , ( "multipleOf", Encode.float 2.0 )
                , ( "maximum", Encode.float 2.0 )
                , ( "exclusiveMaximum", Encode.float 2.0 )
                , ( "minimum", Encode.float 1.0 )
                , ( "exclusiveMinimum", Encode.float 1.0 )
                ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (IntegerSchema
                            withEmptyMeta
                            (NumberValidations
                                (Just 2.0)
                                (Just 2.0)
                                (Just 2.0)
                                (Just 1.0)
                                (Just 1.0)
                            )
                        )
        , test "number schema" <|
            \() ->
                [ ( "type", Encode.string "number" ) ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (FloatSchema
                            withEmptyMeta
                            (NumberValidations
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                            )
                        )
        , test "string schema" <|
            \() ->
                [ ( "type", Encode.string "string" ) ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (StringSchema
                            withEmptyMeta
                            (StringValidations
                                Nothing
                                Nothing
                                Nothing
                            )
                        )
        , test "undefined schema" <|
            \() ->
                []
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (Undefined
                            withEmptyMeta
                            (NumberValidations
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                            )
                            (StringValidations
                                Nothing
                                Nothing
                                Nothing
                            )
                        )
        , test "list of one" <|
            \() ->
                [ ( "type"
                  , Encode.list [ Encode.string "string" ]
                  )
                ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (StringSchema
                            withEmptyMeta
                            (StringValidations
                                Nothing
                                Nothing
                                Nothing
                            )
                        )
        , test "nullable type" <|
            \() ->
                [ ( "type"
                  , [ "string", "null" ]
                        |> List.map Encode.string
                        |> Encode.list
                  )
                ]
                    |> decodeSchema
                    |> shouldResultWithSchema
                        (Undefined
                            withEmptyMeta
                            (NumberValidations
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                            )
                            (StringValidations
                                Nothing
                                Nothing
                                Nothing
                            )
                        )
        ]

withEmptyMeta : Meta
withEmptyMeta =
    Meta Nothing Nothing Nothing Nothing

shouldResultWithSchema : Schema -> Result x Schema -> Expect.Expectation
shouldResultWithSchema s =
    s
        |> Ok
        |> Expect.equal


decodeSchema : List ( String, Value ) -> Result String Schema
decodeSchema list =
    list
        |> Encode.object
        |> JS.fromValue
