module Validations exposing (all)

import Json.Schema.Builder as JSB
    exposing
        ( buildSchema
        , boolSchema
        , withItem
        , withItems
        , withAdditionalItems
        , withContains
        , withProperties
        , withPatternProperties
        , withAdditionalProperties
        , withSchemaDependency
        , withPropNamesDependency
        , withPropertyNames
        , withType
        , withNullableType
        , withUnionType
        , withAllOf
        , withAnyOf
        , withOneOf
        , withMultipleOf
        , withMaximum
        , withMinimum
        , withExclusiveMaximum
        , withExclusiveMinimum
        , withPattern
        , withEnum
        , withRequired
        , withMaxLength
        , withMinLength
        , withMaxProperties
        , withMinProperties
        , withMaxItems
        , withMinItems
        , withUniqueItems
        , withConst
        , validate
        )
import Json.Encode as Encode exposing (int)
import Json.Decode as Decode exposing (decodeValue)
import Json.Schema.Validation as Validation exposing (Error, defaultOptions, JsonPointer, ValidationError(..))
import Json.Schema.Definitions exposing (blankSchema)
import Test exposing (Test, describe, test, only)
import Ref
import Expect


all : Test
all =
    describe "validations"
        [ describe "multipleOf"
            [ test "success with int" <|
                \() ->
                    buildSchema
                        |> withMultipleOf 2
                        |> JSB.validate defaultOptions (Encode.int 4)
                        |> expectOk
            , test "success with float" <|
                \() ->
                    buildSchema
                        |> withMultipleOf 2.1
                        |> JSB.validate defaultOptions (Encode.float 4.2)
                        |> expectOk
            , test "success with periodic float" <|
                \() ->
                    buildSchema
                        |> withMultipleOf (1 / 3)
                        |> JSB.validate defaultOptions (Encode.float (2 / 3))
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withMultipleOf 3
                        |> JSB.validate defaultOptions (Encode.float (2 / 7))
                        |> Expect.equal (Err [ error [] <| MultipleOf 3 (2 / 7) ])
            ]
        , describe "maximum"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withMaximum 2
                        |> JSB.validate defaultOptions (Encode.int 2)
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withMaximum 2
                        |> JSB.validate defaultOptions (Encode.float 2.1)
                        |> Expect.equal (Err [ error [] <| Maximum 2.0 2.1 ])
            ]
        , describe "minimum"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withMinimum 2
                        |> JSB.validate defaultOptions (Encode.int 2)
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withMinimum 2
                        |> JSB.validate defaultOptions (Encode.float 1.9)
                        |> Expect.equal (Err [ error [] <| Minimum 2.0 1.9 ])
            ]
        , describe "exclusiveMaximum"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withExclusiveMaximum 2
                        |> JSB.validate defaultOptions (Encode.float 1.9)
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withExclusiveMaximum 2
                        |> JSB.validate defaultOptions (Encode.float 2)
                        |> Expect.equal (Err [ error [] <| ExclusiveMaximum 2 2 ])
            ]
        , describe "exclusiveMinimum"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withExclusiveMinimum 2
                        |> JSB.validate defaultOptions (Encode.float 2.1)
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withExclusiveMinimum 2
                        |> JSB.validate defaultOptions (Encode.float 2)
                        |> Expect.equal (Err [ error [] <| ExclusiveMinimum 2 2 ])
            ]
        , describe "maxLength"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withMaxLength 3
                        |> JSB.validate defaultOptions (Encode.string "foo")
                        |> expectOk
            , test "success for non-strings" <|
                \() ->
                    buildSchema
                        |> withMaxLength 3
                        |> JSB.validate defaultOptions (Encode.int 10000)
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withMaxLength 2
                        |> validate defaultOptions (Encode.string "foo")
                        |> Expect.equal (Err [ error [] <| MaxLength 2 3 ])
            ]
        , describe "minLength"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withMinLength 3
                        |> validate defaultOptions (Encode.string "foo")
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withMinLength 4
                        |> validate defaultOptions (Encode.string "foo")
                        |> Expect.equal (Err [ error [] <| MinLength 4 3 ])
            ]
        , describe "pattern"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withPattern "o{2}"
                        |> JSB.validate defaultOptions (Encode.string "foo")
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withPattern "o{3}"
                        |> JSB.validate defaultOptions (Encode.string "foo")
                        |> Expect.equal (Err [ error [] <| Pattern "o{3}" "foo" ])
            ]
        , describe "items: schema"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withItem (buildSchema |> withMaximum 10)
                        |> JSB.validate defaultOptions (Encode.list [ int 1 ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withItem (buildSchema |> withMaximum 10)
                        |> JSB.validate defaultOptions (Encode.list [ int 1, int 11 ])
                        |> Expect.equal (Err [ error [ "1" ] <| Maximum 10 11 ])
            ]
        , describe "items: array of schema"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withItems
                            [ buildSchema
                                |> withMaximum 10
                            , buildSchema
                                |> withMaximum 100
                            ]
                        |> JSB.validate defaultOptions (Encode.list [ int 1, int 20 ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withItems
                            [ buildSchema
                                |> withMaximum 11
                            , buildSchema
                                |> withMaximum 100
                            ]
                        |> JSB.validate defaultOptions (Encode.list [ int 100, int 2 ])
                        |> Expect.equal (Err [ error [ "0" ] <| Maximum 11 100 ])
            ]
        , describe "items: array of schema with additional items"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withItems
                            [ buildSchema
                                |> withMaximum 10
                            , buildSchema
                                |> withMaximum 100
                            ]
                        |> withAdditionalItems (buildSchema |> withMaximum 1)
                        |> JSB.validate defaultOptions (Encode.list [ int 1, int 20, int 1 ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withItems
                            [ buildSchema
                                |> withMaximum 11
                            , buildSchema
                                |> withMaximum 100
                            ]
                        |> withAdditionalItems (buildSchema |> withMaximum 1)
                        |> JSB.validate defaultOptions (Encode.list [ int 2, int 2, int 100 ])
                        |> Expect.equal (Err [ error [ "2" ] <| Maximum 1 100 ])
            ]
        , describe "maxItems"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withMaxItems 3
                        |> validate defaultOptions (Encode.list [ int 1, int 2 ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withMaxItems 2
                        |> validate defaultOptions (Encode.list [ int 1, int 2, int 3 ])
                        |> Expect.equal (Err [ error [] <| MaxItems 2 3 ])
            ]
        , describe "minItems"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withMinItems 2
                        |> validate defaultOptions (Encode.list [ int 1, int 2, int 3 ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withMinItems 3
                        |> validate defaultOptions (Encode.list [ int 1, int 2 ])
                        |> Expect.equal (Err [ error [] <| MinItems 3 2 ])
            ]
        , describe "uniqueItems"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withUniqueItems True
                        |> validate defaultOptions (Encode.list [ int 1, int 2, int 3 ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withUniqueItems True
                        |> validate defaultOptions (Encode.list [ int 1, int 1 ])
                        |> Expect.equal (Err [ error [] <| UniqueItems (int 1) ])
            ]
        , describe "contains"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withContains (buildSchema |> withMaximum 1)
                        |> JSB.validate defaultOptions (Encode.list [ int 10, int 20, int 1 ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withContains (buildSchema |> withMaximum 1)
                        |> JSB.validate defaultOptions (Encode.list [ int 10, int 20 ])
                        |> Expect.equal (Err [ error [] Contains ])
            ]
        , describe "maxProperties"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withMaxProperties 3
                        |> validate defaultOptions (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withMaxProperties 1
                        |> validate defaultOptions (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Err [ error [] <| MaxProperties 1 2 ])
            ]
        , describe "minProperties"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withMinProperties 1
                        |> validate defaultOptions (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withMinProperties 3
                        |> validate defaultOptions (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Err [ error [] <| MinProperties 3 2 ])
            ]
        , describe "required"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withRequired [ "foo", "bar" ]
                        |> validate defaultOptions (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withRequired [ "foo", "bar" ]
                        |> validate defaultOptions (Encode.object [ ( "foo", int 1 ) ])
                        |> Expect.equal (Err [ error [] <| Required [ "bar" ], error [ "bar" ] RequiredProperty ])
            ]
        , describe "properties"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withProperties
                            [ ( "foo", buildSchema |> withMaximum 10 )
                            , ( "bar", buildSchema |> withMaximum 20 )
                            ]
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withProperties
                            [ ( "foo", buildSchema |> withMaximum 10 )
                            , ( "bar", buildSchema |> withMaximum 20 )
                            ]
                        |> JSB.validate defaultOptions (Encode.object [ ( "bar", int 28 ) ])
                        |> Expect.equal (Err [ error [ "bar" ] <| Maximum 20 28 ])
            ]
        , describe "patternProperties"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withPatternProperties
                            [ ( "o{2}", buildSchema |> withMaximum 10 )
                            , ( "a", buildSchema |> withMaximum 20 )
                            ]
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withPatternProperties
                            [ ( "o{2}", buildSchema |> withMaximum 10 )
                            , ( "a", buildSchema |> withMaximum 20 )
                            ]
                        |> JSB.validate defaultOptions (Encode.object [ ( "bar", int 28 ) ])
                        |> Expect.equal (Err [ error [ "bar" ] <| Maximum 20 28 ])
            ]
        , describe "additionalProperties"
            [ test "success: pattern" <|
                \() ->
                    buildSchema
                        |> withPatternProperties
                            [ ( "o{2}", buildSchema |> withMaximum 100 )
                            ]
                        |> withAdditionalProperties (buildSchema |> withMaximum 20)
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 100 ), ( "bar", int 2 ) ])
                        |> expectOk
            , test "success: props" <|
                \() ->
                    buildSchema
                        |> withProperties
                            [ ( "foo", buildSchema |> withMaximum 100 )
                            ]
                        |> withAdditionalProperties (buildSchema |> withMaximum 20)
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 100 ), ( "bar", int 2 ) ])
                        |> expectOk
            , test "success: boolean true" <|
                \() ->
                    buildSchema
                        |> withProperties
                            [ ( "foo", buildSchema |> withMaximum 100 )
                            ]
                        |> withAdditionalProperties (boolSchema True)
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 100 ), ( "bar", int 2 ) ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withPatternProperties
                            [ ( "o{2}", buildSchema |> withMaximum 100 )
                            ]
                        |> withAdditionalProperties (buildSchema |> withMaximum 20)
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 100 ), ( "bar", int 200 ) ])
                        |> Expect.equal (Err [ error [ "bar" ] <| Maximum 20 200 ])
            , test "success: boolean false" <|
                \() ->
                    buildSchema
                        |> withPatternProperties
                            [ ( "o{2}", buildSchema |> withMaximum 100 )
                            ]
                        |> withAdditionalProperties (boolSchema False)
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 100 ) ])
                        |> expectOk
            , test "failure: boolean false" <|
                \() ->
                    buildSchema
                        |> withPatternProperties
                            [ ( "o{2}", buildSchema |> withMaximum 100 )
                            ]
                        |> withAdditionalProperties (boolSchema False)
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 100 ), ( "bar", int 200 ) ])
                        |> Expect.equal (Err [ error [] <| AdditionalPropertiesDisallowed [ "bar" ], error [ "bar" ] AdditionalPropertyDisallowed ])
            ]
        , describe "dependencies"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withSchemaDependency
                            "foo"
                            (buildSchema |> withRequired [ "bar" ])
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> expectOk
            , test "failure when dependency is a schema" <|
                \() ->
                    buildSchema
                        |> withSchemaDependency
                            "foo"
                            (buildSchema |> withRequired [ "bar" ])
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 1 ) ])
                        |> Expect.equal (Err [ error [] <| Required [ "bar" ], error [ "bar" ] RequiredProperty ])
              --|> Expect.equal (Err "Required property 'bar' is missing")
            , test "failure when dependency is array of strings" <|
                \() ->
                    buildSchema
                        |> withPropNamesDependency "foo" [ "bar" ]
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 1 ) ])
                        |> Expect.equal (Err [ error [] <| Required [ "bar" ], error [ "bar" ] RequiredProperty ])
            ]
        , describe "propertyNames"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withPropertyNames (buildSchema |> withPattern "^ba")
                        |> JSB.validate defaultOptions (Encode.object [ ( "baz", int 1 ), ( "bar", int 2 ) ])
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withPropertyNames (buildSchema |> withPattern "^ba")
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Err [ error [] <| InvalidPropertyName [ error [ "foo" ] <| Pattern "^ba" "foo" ] ])
            ]
        , describe "enum"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withEnum [ int 1, int 2 ]
                        |> validate defaultOptions (Encode.int 2)
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withEnum [ int 1, int 2 ]
                        |> validate defaultOptions (Encode.int 3)
                        |> Expect.equal (Err [ error [] Enum ])
            ]
        , describe "const"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withConst (int 1)
                        |> validate defaultOptions (Encode.int 1)
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withConst (int 1)
                        |> validate defaultOptions (Encode.int 2)
                        |> Expect.equal (Err [ error [] Const ])
            ]
        , describe "type=string"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withType "string"
                        |> JSB.validate defaultOptions (Encode.string "foo")
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withType "string"
                        |> JSB.validate defaultOptions (Encode.int 1)
                        |> Expect.equal (Err [ error [] <| InvalidType "Expecting a String but instead got: 1" ])
            ]
        , describe "type=number"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withType "number"
                        |> JSB.validate defaultOptions (Encode.int 1)
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withType "number"
                        |> JSB.validate defaultOptions (Encode.string "bar")
                        |> Expect.equal (Err [ error [] <| InvalidType "Expecting a Float but instead got: \"bar\"" ])
            , test "failure with null" <|
                \() ->
                    buildSchema
                        |> withType "number"
                        |> JSB.validate defaultOptions Encode.null
                        |> Expect.equal (Err [ error [] <| InvalidType "Expecting a Float but instead got: null" ])
            ]
        , describe "type=null,number"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withNullableType "number"
                        |> JSB.validate defaultOptions (Encode.int 1)
                        |> expectOk
            , test "success with null" <|
                \() ->
                    buildSchema
                        |> withNullableType "number"
                        |> JSB.validate defaultOptions Encode.null
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withNullableType "number"
                        |> JSB.validate defaultOptions (Encode.string "bar")
                        |> Expect.equal (Err [ error [] <| InvalidType "Expecting a Float but instead got: \"bar\"" ])
            ]
        , describe "type=number,string"
            [ test "success for number" <|
                \() ->
                    buildSchema
                        |> withUnionType [ "number", "string" ]
                        |> JSB.validate defaultOptions (Encode.int 1)
                        |> expectOk
            , test "success for string" <|
                \() ->
                    buildSchema
                        |> withUnionType [ "number", "string" ]
                        |> JSB.validate defaultOptions (Encode.string "str")
                        |> expectOk
            , test "failure for object" <|
                \() ->
                    buildSchema
                        |> withUnionType [ "number", "string" ]
                        |> JSB.validate defaultOptions (Encode.object [])
                        |> Expect.equal (Err [ error [] <| InvalidType "None of desired types match" ])
            ]
        , describe "allOf"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withAllOf
                            [ buildSchema |> withMinimum 0
                            , buildSchema |> withMaximum 1
                            ]
                        |> JSB.validate defaultOptions (Encode.int 1)
                        |> expectOk
            , test "failure because of minimum" <|
                \() ->
                    buildSchema
                        |> withAllOf
                            [ buildSchema |> withMinimum 0
                            , buildSchema |> withMaximum 1
                            ]
                        |> JSB.validate defaultOptions (Encode.int -1)
                        |> Expect.equal (Err [ error [] <| Minimum 0 -1 ])
            , test "failure because of maximum" <|
                \() ->
                    buildSchema
                        |> withAllOf
                            [ buildSchema |> withMinimum 0
                            , buildSchema |> withMaximum 1
                            ]
                        |> JSB.validate defaultOptions (Encode.int 2)
                        |> Expect.equal (Err [ error [] <| Maximum 1 2 ])
            ]
        , describe "anyOf"
            [ test "success for enum" <|
                \() ->
                    buildSchema
                        |> withAllOf
                            [ buildSchema |> withMinimum 0
                            , buildSchema |> withEnum [ int 1 ]
                            ]
                        |> JSB.validate defaultOptions (Encode.int 1)
                        |> expectOk
            , test "success for minimum" <|
                \() ->
                    buildSchema
                        |> withAnyOf
                            [ buildSchema |> withMinimum 0
                            , buildSchema |> withEnum [ int 1 ]
                            ]
                        |> JSB.validate defaultOptions (Encode.float 0.5)
                        |> expectOk
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withAnyOf
                            [ buildSchema |> withMinimum 0
                            , buildSchema |> withEnum [ int 1 ]
                            ]
                        |> JSB.validate defaultOptions (Encode.int -1)
                        |> Expect.equal
                            (Err
                                [ error [] <| Minimum 0 -1
                                , error [] <| Enum
                                ]
                            )
            ]
        , describe "oneOf"
            [ test "success for enum" <|
                \() ->
                    buildSchema
                        |> withOneOf
                            [ buildSchema |> withMinimum 10
                            , buildSchema |> withEnum [ int 1 ]
                            ]
                        |> JSB.validate defaultOptions (Encode.int 1)
                        |> expectOk
            , test "success for minimum" <|
                \() ->
                    buildSchema
                        |> withOneOf
                            [ buildSchema |> withMinimum 0
                            , buildSchema |> withEnum [ int 1 ]
                            ]
                        |> JSB.validate defaultOptions (Encode.int 0)
                        |> expectOk
            , test "failure for all" <|
                \() ->
                    buildSchema
                        |> withOneOf
                            [ buildSchema |> withMinimum 0
                            , buildSchema |> withEnum [ int 1 ]
                            ]
                        |> JSB.validate defaultOptions (Encode.int -1)
                        |> Expect.equal (Err [ error [] OneOfNoneSucceed ])
            , test "failure because of success for both" <|
                \() ->
                    buildSchema
                        |> withOneOf
                            [ buildSchema |> withMinimum 0
                            , buildSchema |> withEnum [ int 1 ]
                            ]
                        |> JSB.validate defaultOptions (Encode.int 1)
                        |> Expect.equal (Err [ error [] <| OneOfManySucceed 2 ])
            ]
        , describe "boolean schema"
            [ test "true always validates any value" <|
                \() ->
                    Encode.bool True
                        |> decodeValue Json.Schema.Definitions.decoder
                        |> Result.withDefault blankSchema
                        |> (\s -> Validation.validate defaultOptions Ref.defaultPool (int 1) s s)
                        |> expectOk
            , test "false always fails validation" <|
                \() ->
                    Encode.bool False
                        |> decodeValue Json.Schema.Definitions.decoder
                        |> Result.withDefault blankSchema
                        |> (\s -> Validation.validate defaultOptions Ref.defaultPool (int 1) s s)
                        |> Expect.equal (Err [ error [] AlwaysFail ])
            ]
        , describe "multiple errors"
            [ test "validation should return multiple errors" <|
                \() ->
                    buildSchema
                        |> withProperties
                            [ ( "foo", buildSchema |> withMaximum 1 )
                            , ( "bar", buildSchema |> withMaximum 2 )
                            ]
                        |> JSB.validate defaultOptions (Encode.object [ ( "foo", int 7 ), ( "bar", int 28 ) ])
                        |> Expect.equal
                            (Err
                                [ error [ "foo" ] <| Maximum 1 7
                                , error [ "bar" ] <| Maximum 2 28
                                ]
                            )
            ]
        ]


error : List String -> ValidationError -> Error
error path =
    Error (JsonPointer "" path)


expectOk : Result x a -> Expect.Expectation
expectOk e =
    case e of
        Err x ->
            Expect.fail <| "Unexpected error: " ++ (toString x)

        Ok _ ->
            Expect.pass
