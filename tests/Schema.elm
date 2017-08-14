module Schema exposing (all)

import Json.Schema as JS exposing (Schema, empty)


--import Json.Decode as Decode exposing (decodeString, value)

import Json.Encode as Encode exposing (Value, string, int, object, list)
import Test exposing (..)
import Expect
import Data.Schema
    exposing
        ( Type(SingleType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        )


blankRoot : Value
blankRoot =
    object []


schema : String -> Schema
schema str =
    JS.fromString str |> Result.withDefault empty


skip : String -> b -> Test
skip a b =
    test a (\_ -> Expect.equal True True)


all : Test
all =
    describe "Json Schema"
        [ describe "Creation"
            [ test "from a valid json string" <|
                \() ->
                    JS.fromString "{}"
                        |> Expect.equal (Ok empty)
            , test "from string which is not a valid json" <|
                \() ->
                    JS.fromString ""
                        |> Expect.equal (Err "Given an invalid JSON: Unexpected end of JSON input")
            , test "from value which is an empty object" <|
                \() ->
                    JS.fromValue (object [])
                        |> Expect.equal (Ok empty)
            , test "has enum: type should become a string" <|
                \() ->
                    schema """
                        { "enum": [ "a", "b" ] }
                    """
                        |> .type_
                        |> Expect.equal (SingleType StringType)
            , test "has properties: type should become an object" <|
                \() ->
                    schema """
                        { "properties": { "a": { "type": "string" } } }
                    """
                        |> .type_
                        |> Expect.equal (SingleType ObjectType)
            ]
        , describe "Manipulation"
            [ test "simple object with string" <|
                \() ->
                    blankRoot
                        |> JS.setValue simpleSchema [ "foo" ] (string "bar")
                        |> Expect.equal (Ok (object [ ( "foo", string "bar" ) ]))
            , test "simple object with int" <|
                \() ->
                    blankRoot
                        |> JS.setValue simpleIntSchema [ "foo" ] (int 0)
                        |> Result.withDefault blankRoot
                        |> JS.getValue simpleIntSchema [ "foo" ]
                        |> Expect.equal (int 0)
            , test "nested object" <|
                \() ->
                    blankRoot
                        |> Expect.equal (object [ ( "foo", object [ ( "bar", string "baz" ) ] ) ])
            , test "nested array" <|
                \() ->
                    let
                        set path val target =
                            JS.setValue nestedArraySchema path val target
                                |> Result.withDefault blankRoot

                        get =
                            JS.getString nestedArraySchema
                    in
                        blankRoot
                            |> set [ "fooes", "0" ] (string "bam")
                            |> set [ "fooes", "1" ] (string "ban")
                            |> set [ "fooes", "2" ] (string "nam")
                            |> get [ "fooes", "1" ]
                            |> Expect.equal "ban"
            , test "nested array of objects" <|
                \() ->
                    let
                        set path val target =
                            JS.setValue nestedArrayObjectSchema path val target
                                |> Result.withDefault blankRoot

                        get =
                            JS.getString nestedArrayObjectSchema
                    in
                        blankRoot
                            |> set [ "fooes", "0", "baz" ] (string "baa")
                            |> set [ "fooes", "1", "baz" ] (string "bah")
                            |> set [ "fooes", "2", "baz" ] (string "huh")
                            |> get [ "fooes", "1", "baz" ]
                            |> Expect.equal "bah"
            , test "error when property does not exist" <|
                \() ->
                    let
                        set =
                            JS.setValue simpleSchema
                    in
                        blankRoot
                            |> set [ "some", "wrong", "path" ] (string "x")
                            |> Expect.equal (Err "Key 'some' not found")
            , test "error when property type does not match" <|
                \() ->
                    blankRoot
                        |> JS.setValue simpleSchema [ "foo" ] (int 647)
                        |> Expect.equal (Err "Expecting a String but instead got: 647")
            ]
        ]


simpleSchema : Schema
simpleSchema =
    schema """
        { "properties": { "foo": { "type": "string" } }
        }
    """


simpleIntSchema : Schema
simpleIntSchema =
    schema """
        { "properties": { "foo": { "type": "integer" } }
        }
    """


nestedSchema : Schema
nestedSchema =
    schema """
        { "type": "object"
        , "properties":
            { "foo":
                { "type": "object"
                , "properties" : { "bar": { "type": "string" } }
                }
            }
        }
    """


nestedArraySchema : Schema
nestedArraySchema =
    schema """
        { "type": "object"
        , "properties":
            { "fooes":
                { "type": "array"
                , "items": { "type": "string" }
                }
            }
        }
    """


nestedArrayObjectSchema : Schema
nestedArrayObjectSchema =
    schema """
        { "type": "object"
        , "properties":
            { "fooes":
                { "type": "array"
                , "items":
                    { "type": "object"
                    , "properties": { "baz": { "type": "string" } }
                    }
                }
            }
        }
    """
