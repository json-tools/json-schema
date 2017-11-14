module Generate exposing (all)

import Test exposing (Test, describe, test, only)
import Json.Schema.Builder
    exposing
        ( SchemaBuilder
        , buildSchema
        , boolSchema
        , toSchema
        , withType
        , withNullableType
        , withUnionType
        , withMinimum
        , withMaximum
        , withContains
        , withDefinitions
        , withItems
        , withItem
        , withMaxItems
        , withMinItems
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
        , withTitle
        , withNot
        , withExamples
        , withEnum
        )
import Json.Schema.Definitions as Schema exposing (Schema, decoder, blankSchema)
import Json.Schema.Random as JSR
import Expect
import Json.Encode as Encode exposing (Value)
import Random exposing (initialSeed, step)


all : Test
all =
    describe "random value generator"
        [ describe "generate by example"
            [ test "enough examples" <|
                \() ->
                    buildSchema
                        |> withExamples
                            [ Encode.string "dime"
                            , Encode.int 2
                            , Encode.int 3
                            , Encode.int 4
                            , Encode.int 5
                            ]
                        |> toSchema
                        |> Result.withDefault (blankSchema)
                        |> JSR.value JSR.defaultSettings
                        |> flip step (initialSeed 178)
                        |> (\( v, _ ) -> Expect.equal v (Encode.string "dime"))
            , test "not enough examples" <|
                \() ->
                    buildSchema
                        |> withExamples []
                        |> toSchema
                        |> Result.withDefault (blankSchema)
                        |> JSR.value JSR.defaultSettings
                        |> flip step (initialSeed 178)
                        |> (\( v, _ ) -> Expect.equal v Encode.null)
            ]
        , describe "generate by enum"
            [ test "enough enums" <|
                \() ->
                    buildSchema
                        |> withEnum
                            [ Encode.string "dime"
                            , Encode.int 2
                            , Encode.int 3
                            , Encode.int 4
                            , Encode.int 5
                            ]
                        |> toSchema
                        |> Result.withDefault (blankSchema)
                        |> JSR.value JSR.defaultSettings
                        |> flip step (initialSeed 178)
                        |> (\( v, _ ) -> Expect.equal v (Encode.string "dime"))
            , test "not enough examples" <|
                \() ->
                    buildSchema
                        |> withEnum []
                        |> toSchema
                        |> Result.withDefault (blankSchema)
                        |> JSR.value JSR.defaultSettings
                        |> flip step (initialSeed 178)
                        |> (\( v, _ ) -> Expect.equal v Encode.null)
            ]
        , describe "random object generation"
            [ test "object with required fields" <|
                \() ->
                    buildSchema
                        |> withProperties
                            [ ( "foo", buildSchema |> withType "integer" ) ]
                        |> toSchema
                        |> Result.withDefault (blankSchema)
                        |> JSR.value JSR.defaultSettings
                        |> flip step (initialSeed 2)
                        |> (\( v, _ ) -> Expect.equal v (Encode.object [ ( "foo", Encode.int 688281600 ) ]))
            ]
        , describe "random array generation"
            [ test "list of similar items" <|
                \() ->
                    buildSchema
                        |> withItem (buildSchema |> withType "integer" |> withMinimum 0 |> withMaximum 10)
                        |> withMaxItems 10
                        |> toSchema
                        |> Result.withDefault (blankSchema)
                        |> JSR.value JSR.defaultSettings
                        |> flip step (initialSeed 1)
                        |> (\( v, _ ) ->
                                [ 3, 9, 7 ]
                                    |> List.map Encode.int
                                    |> Encode.list
                                    |> Expect.equal v
                           )
            ]
        ]
