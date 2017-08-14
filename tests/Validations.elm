module Validations exposing (all)

import Json.Schema.Builder as JSB
    exposing
        ( buildSchema
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
        , withMaximum
        , withPattern
        )
import Data.Schema exposing (blankSchema)
import Validation exposing (validate)
import Json.Encode as Encode exposing (int)
import Test exposing (Test, describe, test)
import Expect


all : Test
all =
    describe "validations"
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
                    { blankSchema | multipleOf = Just (1 / 3) }
                        |> validate (Encode.float (2 / 3))
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    { blankSchema | multipleOf = Just 3 }
                        |> validate (Encode.float (2 / 7))
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
        , describe "items: schema"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withItem { blankSchema | maximum = Just 10 }
                        |> JSB.validate (Encode.list [ int 1 ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withItem { blankSchema | maximum = Just 10 }
                        |> JSB.validate (Encode.list [ int 1, int 11 ])
                        |> Expect.equal (Err "Item at index 1: Value is above the maximum of 10")
            ]
        , describe "items: array of schema"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withItems
                            [ { blankSchema | maximum = Just 10 }
                            , { blankSchema | maximum = Just 100 }
                            ]
                        |> JSB.validate (Encode.list [ int 1, int 20 ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withItems
                            [ { blankSchema | maximum = Just 11 }
                            , { blankSchema | maximum = Just 100 }
                            ]
                        |> JSB.validate (Encode.list [ int 100, int 2 ])
                        |> Expect.equal (Err "Item at index 0: Value is above the maximum of 11")
            ]
        , describe "items: array of schema with additional items"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withItems
                            [ { blankSchema | maximum = Just 10 }
                            , { blankSchema | maximum = Just 100 }
                            ]
                        |> withAdditionalItems ( buildSchema |> withMaximum 1 )
                        |> JSB.validate (Encode.list [ int 1, int 20, int 1 ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withItems
                            [ { blankSchema | maximum = Just 11 }
                            , { blankSchema | maximum = Just 100 }
                            ]
                        |> withAdditionalItems ( buildSchema |> withMaximum 1 )
                        |> JSB.validate (Encode.list [ int 2, int 2, int 100 ])
                        |> Expect.equal (Err "Item at index 2: Value is above the maximum of 1")
            ]
        , describe "maxItems"
            [ test "success" <|
                \() ->
                    { blankSchema | maxItems = Just 3 }
                        |> validate (Encode.list [ int 1, int 2 ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    { blankSchema | maxItems = Just 2 }
                        |> validate (Encode.list [ int 1, int 2, int 3 ])
                        |> Expect.equal (Err "Array has more items than expected (maxItems=2)")
            ]
        , describe "minItems"
            [ test "success" <|
                \() ->
                    { blankSchema | minItems = Just 2 }
                        |> validate (Encode.list [ int 1, int 2, int 3 ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    { blankSchema | minItems = Just 3 }
                        |> validate (Encode.list [ int 1, int 2 ])
                        |> Expect.equal (Err "Array has less items than expected (minItems=3)")
            ]
        , describe "uniqueItems"
            [ test "success" <|
                \() ->
                    { blankSchema | uniqueItems = Just True }
                        |> validate (Encode.list [ int 1, int 2, int 3 ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    { blankSchema | uniqueItems = Just True }
                        |> validate (Encode.list [ int 1, int 1 ])
                        |> Expect.equal (Err "Array has not unique items")
            ]
        , describe "contains"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withContains ( buildSchema |> withMaximum 1 )
                        |> JSB.validate (Encode.list [ int 10, int 20, int 1 ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withContains ( buildSchema |> withMaximum 1 )
                        |> JSB.validate (Encode.list [ int 10, int 20 ])
                        |> Expect.equal (Err "Array does not contain expected value")
            ]
        , describe "maxProperties"
            [ test "success" <|
                \() ->
                    { blankSchema | maxProperties = Just 3 }
                        |> validate (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    { blankSchema | maxProperties = Just 1 }
                        |> validate (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Err "Object has more properties than expected (maxProperties=1)")
            ]
        , describe "minProperties"
            [ test "success" <|
                \() ->
                    { blankSchema | minProperties = Just 1 }
                        |> validate (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    { blankSchema | minProperties = Just 3 }
                        |> validate (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Err "Object has less properties than expected (minProperties=3)")
            ]
        , describe "required"
            [ test "success" <|
                \() ->
                    { blankSchema | required = Just [ "foo", "bar" ] }
                        |> validate (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    { blankSchema | required = Just [ "foo", "bar" ] }
                        |> validate (Encode.object [ ( "foo", int 1 ) ])
                        |> Expect.equal (Err "Object doesn't have all the required properties")
            ]
        , describe "properties"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withProperties
                            [ ( "foo", { blankSchema | maximum = Just 10 } )
                            , ( "bar", { blankSchema | maximum = Just 20 } )
                            ]
                        |> JSB.validate (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withProperties
                            [ ( "foo", { blankSchema | maximum = Just 10 } )
                            , ( "bar", { blankSchema | maximum = Just 20 } )
                            ]
                        |> JSB.validate (Encode.object [ ( "bar", int 28 ) ])
                        |> Expect.equal (Err "Invalid property 'bar': Value is above the maximum of 20")
            ]
        , describe "patternProperties"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withPatternProperties
                            [ ( "o{2}", { blankSchema | maximum = Just 10 } )
                            , ( "a", { blankSchema | maximum = Just 20 } )
                            ]
                        |> JSB.validate (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withPatternProperties
                            [ ( "o{2}", { blankSchema | maximum = Just 10 } )
                            , ( "a", { blankSchema | maximum = Just 20 } )
                            ]
                        |> JSB.validate (Encode.object [ ( "bar", int 28 ) ])
                        |> Expect.equal (Err "Invalid property 'bar': Value is above the maximum of 20")
            ]
        , describe "additionalProperties"
            [ test "success: pattern" <|
                \() ->
                    buildSchema
                        |> withPatternProperties
                            [ ( "o{2}", { blankSchema | maximum = Just 100 } )
                            ]
                        |> withAdditionalProperties ( buildSchema |> withMaximum 20 )
                        |> JSB.validate (Encode.object [ ( "foo", int 100 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Ok True)
            , test "success: props" <|
                \() ->
                    buildSchema
                        |> withProperties
                            [ ( "foo", { blankSchema | maximum = Just 100 } )
                            ]
                        |> withAdditionalProperties ( buildSchema |> withMaximum 20 )
                        |> JSB.validate (Encode.object [ ( "foo", int 100 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withPatternProperties
                            [ ( "o{2}", { blankSchema | maximum = Just 100 } )
                            ]
                        |> withAdditionalProperties ( buildSchema |> withMaximum 20 )
                        |> JSB.validate (Encode.object [ ( "foo", int 100 ), ( "bar", int 200 ) ])
                        |> Expect.equal (Err "Invalid property 'bar': Value is above the maximum of 20")
            ]
        , describe "dependencies"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withSchemaDependency
                            "foo"
                            { blankSchema | required = Just [ "bar" ] }
                        |> JSB.validate (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Ok True)
            , test "failure when dependency is a schema" <|
                \() ->
                    buildSchema
                        |> withSchemaDependency
                            "foo"
                            { blankSchema | required = Just [ "bar" ] }
                        |> JSB.validate (Encode.object [ ( "foo", int 1 ) ])
                        |> Expect.equal (Err "Object doesn't have all the required properties")
              --|> Expect.equal (Err "Required property 'bar' is missing")
            , test "failure when dependency is array of strings" <|
                \() ->
                    buildSchema
                        |> withPropNamesDependency "foo" [ "bar" ]
                        |> JSB.validate (Encode.object [ ( "foo", int 1 ) ])
                        |> Expect.equal (Err "Object doesn't have all the required properties")
            ]
        , describe "propertyNames"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withPropertyNames ( buildSchema |> withPattern "^ba" )
                        |> JSB.validate (Encode.object [ ( "baz", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withPropertyNames ( buildSchema |> withPattern "^ba" )
                        |> JSB.validate (Encode.object [ ( "foo", int 1 ), ( "bar", int 2 ) ])
                        |> Expect.equal (Err "Property 'foo' doesn't validate against peopertyNames schema: String does not match the regex pattern")
            ]
        , describe "enum"
            [ test "success" <|
                \() ->
                    { blankSchema | enum = Just [ int 1, int 2 ] }
                        |> validate (Encode.int 2)
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    { blankSchema | enum = Just [ int 1, int 2 ] }
                        |> validate (Encode.int 3)
                        |> Expect.equal (Err "Value is not present in enum")
            ]
        , describe "const"
            [ test "success" <|
                \() ->
                    { blankSchema | const = Just (int 1) }
                        |> validate (Encode.int 1)
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    { blankSchema | const = Just (int 1) }
                        |> validate (Encode.int 2)
                        |> Expect.equal (Err "Value doesn't equal const: expected \"1\" but the actual value is \"2\"")
            ]
        , describe "type=string"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withType "string"
                        |> JSB.validate (Encode.string "foo")
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withType "string"
                        |> JSB.validate (Encode.int 1)
                        |> Expect.equal (Err "Expecting a String but instead got: 1")
            ]
        , describe "type=number"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withType "number"
                        |> JSB.validate (Encode.int 1)
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withType "number"
                        |> JSB.validate (Encode.string "bar")
                        |> Expect.equal (Err "Expecting a Float but instead got: \"bar\"")
            , test "failure with null" <|
                \() ->
                    buildSchema
                        |> withType "number"
                        |> JSB.validate Encode.null
                        |> Expect.equal (Err "Expecting a Float but instead got: null")
            ]
        , describe "type=null,number"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withNullableType "number"
                        |> JSB.validate (Encode.int 1)
                        |> Expect.equal (Ok True)
            , test "success with null" <|
                \() ->
                    buildSchema
                        |> withNullableType "number"
                        |> JSB.validate Encode.null
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withNullableType "number"
                        |> JSB.validate (Encode.string "bar")
                        |> Expect.equal (Err "Expecting a Float but instead got: \"bar\"")
            ]
        , describe "type=number,string"
            [ test "success for number" <|
                \() ->
                    buildSchema
                        |> withUnionType [ "number", "string" ]
                        |> JSB.validate (Encode.int 1)
                        |> Expect.equal (Ok True)
            , test "success for string" <|
                \() ->
                    buildSchema
                        |> withUnionType [ "number", "string" ]
                        |> JSB.validate (Encode.string "str")
                        |> Expect.equal (Ok True)
            , test "failure for object" <|
                \() ->
                    buildSchema
                        |> withUnionType [ "number", "string" ]
                        |> JSB.validate (Encode.object [])
                        |> Expect.equal (Err "Type mismatch")
            ]
        , describe "allOf"
            [ test "success" <|
                \() ->
                    buildSchema
                        |> withAllOf
                            [ { blankSchema | minimum = Just 0 }
                            , { blankSchema | maximum = Just 1 }
                            ]
                        |> JSB.validate (Encode.int 1)
                        |> Expect.equal (Ok True)
            , test "failure because of minimum" <|
                \() ->
                    buildSchema
                        |> withAllOf
                            [ { blankSchema | minimum = Just 0 }
                            , { blankSchema | maximum = Just 1 }
                            ]
                        |> JSB.validate (Encode.int -1)
                        |> Expect.equal (Err "Value is below the minimum of 0")
            , test "failure because of maximum" <|
                \() ->
                    buildSchema
                        |> withAllOf
                            [ { blankSchema | minimum = Just 0 }
                            , { blankSchema | maximum = Just 1 }
                            ]
                        |> JSB.validate (Encode.int 2)
                        |> Expect.equal (Err "Value is above the maximum of 1")
            ]
        , describe "anyOf"
            [ test "success for enum" <|
                \() ->
                    buildSchema
                        |> withAllOf
                            [ { blankSchema | minimum = Just 0 }
                            , { blankSchema | enum = Just [ int 1 ] }
                            ]
                        |> JSB.validate (Encode.int 1)
                        |> Expect.equal (Ok True)
            , test "success for minimum" <|
                \() ->
                    buildSchema
                        |> withAnyOf
                            [ { blankSchema | minimum = Just 0 }
                            , { blankSchema | enum = Just [ int 1 ] }
                            ]
                        |> JSB.validate (Encode.float 0.5)
                        |> Expect.equal (Ok True)
            , test "failure" <|
                \() ->
                    buildSchema
                        |> withAnyOf
                            [ { blankSchema | minimum = Just 0 }
                            , { blankSchema | enum = Just [ int 1 ] }
                            ]
                        |> JSB.validate (Encode.int -1)
                        |> Expect.equal (Err "None of the schemas in anyOf accept this value")
            ]
        , describe "oneOf"
            [ test "success for enum" <|
                \() ->
                    buildSchema
                        |> withOneOf
                            [ { blankSchema | minimum = Just 10 }
                            , { blankSchema | enum = Just [ int 1 ] }
                            ]
                        |> JSB.validate (Encode.int 1)
                        |> Expect.equal (Ok True)
            , test "success for minimum" <|
                \() ->
                    buildSchema
                        |> withOneOf
                            [ { blankSchema | minimum = Just 0 }
                            , { blankSchema | enum = Just [ int 1 ] }
                            ]
                        |> JSB.validate (Encode.int 0)
                        |> Expect.equal (Ok True)
            , test "failure for all" <|
                \() ->
                    buildSchema
                        |> withOneOf
                            [ { blankSchema | minimum = Just 0 }
                            , { blankSchema | enum = Just [ int 1 ] }
                            ]
                        |> JSB.validate (Encode.int -1)
                        |> Expect.equal (Err "None of the schemas in anyOf allow this value")
            , test "failure because of success for both" <|
                \() ->
                    buildSchema
                        |> withOneOf
                            [ { blankSchema | minimum = Just 0 }
                            , { blankSchema | enum = Just [ int 1 ] }
                            ]
                        |> JSB.validate (Encode.int 1)
                        |> Expect.equal (Err "oneOf expects value to succeed validation against exactly one schema but 2 validations succeeded")
            ]
        ]
