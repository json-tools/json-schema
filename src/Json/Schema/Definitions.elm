module Json.Schema.Definitions
    exposing
        ( Schema(ObjectSchema, BooleanSchema)
        , Schemata(Schemata)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        , stringToType
        , blankSchema
        , blankSubSchema
        , decoder
        , encode
        , SubSchema
        , Items(ItemDefinition, ArrayOfItems, NoItems)
        , Dependency(ArrayPropNames, PropSchema)
        )

{-|

This module contains low-level structures JSON Schema build from.
Normally you wouldn't need to use any of those definitions.

If you really need this low-level API you might need [JSON Schema spec](http://json-schema.org/documentation.html) as guidance.

Feel free to open [issue](https://github.com/1602/json-schema) to describe your use-case, it will affect development roadmap of this library.

# Definitions

@docs Schema, SubSchema, Schemata, Items, Dependency, Type, SingleType, blankSchema, blankSubSchema

# Decoding / encoding

@docs decoder, encode

# Misc

@docs stringToType

-}

import Util exposing (resultToDecoder, foldResults, isInt)
import Json.Decode.Pipeline exposing (decode, optional)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Value, Decoder, succeed, fail, lazy, nullable, andThen, string, float, int, bool, list, value)


{-|
Schema can be either boolean or actual object containing validation and meta properties
-}
type Schema
    = BooleanSchema Bool
    | ObjectSchema SubSchema


{-|
This object holds all draft-6 schema properties
-}
type alias SubSchema =
    { type_ : Type
    , id : Maybe String
    , ref :
        Maybe String
        -- meta
    , title : Maybe String
    , description : Maybe String
    , default : Maybe Value
    , examples : Maybe (List Value)
    , definitions :
        Maybe Schemata
        -- numeric validations
    , multipleOf : Maybe Float
    , maximum : Maybe Float
    , exclusiveMaximum : Maybe Float
    , minimum : Maybe Float
    , exclusiveMinimum :
        Maybe Float
        -- string validations
    , maxLength : Maybe Int
    , minLength : Maybe Int
    , pattern : Maybe String
    , format :
        Maybe String
        -- array validations
    , items : Items
    , additionalItems : Maybe Schema
    , maxItems : Maybe Int
    , minItems : Maybe Int
    , uniqueItems : Maybe Bool
    , contains : Maybe Schema
    , maxProperties : Maybe Int
    , minProperties : Maybe Int
    , required : Maybe (List String)
    , properties : Maybe Schemata
    , patternProperties : Maybe Schemata
    , additionalProperties : Maybe Schema
    , dependencies : List ( String, Dependency )
    , propertyNames : Maybe Schema
    , enum : Maybe (List Value)
    , const : Maybe Value
    , allOf : Maybe (List Schema)
    , anyOf : Maybe (List Schema)
    , oneOf : Maybe (List Schema)
    , not : Maybe Schema
    }


{-|
List of schema-properties used in properties, definitions and patternProperties
-}
type Schemata
    = Schemata (List ( String, Schema ))


{-|
Items definition.
-}
type Items
    = NoItems
    | ItemDefinition Schema
    | ArrayOfItems (List Schema)


{-|
Dependency definition.
-}
type Dependency
    = ArrayPropNames (List String)
    | PropSchema Schema


{-| Create blank JSON Schema `{}`.
-}
blankSchema : Schema
blankSchema =
    ObjectSchema blankSubSchema


{-|
-}
blankSubSchema : SubSchema
blankSubSchema =
    { type_ = AnyType
    , id = Nothing
    , ref = Nothing
    , title = Nothing
    , description = Nothing
    , default = Nothing
    , examples = Nothing
    , definitions = Nothing
    , multipleOf = Nothing
    , maximum = Nothing
    , exclusiveMaximum = Nothing
    , minimum = Nothing
    , exclusiveMinimum = Nothing
    , maxLength = Nothing
    , minLength = Nothing
    , pattern = Nothing
    , format = Nothing
    , items = NoItems
    , additionalItems = Nothing
    , maxItems = Nothing
    , minItems = Nothing
    , uniqueItems = Nothing
    , contains = Nothing
    , maxProperties = Nothing
    , minProperties = Nothing
    , required = Nothing
    , properties = Nothing
    , patternProperties = Nothing
    , additionalProperties = Nothing
    , dependencies = []
    , propertyNames = Nothing
    , enum = Nothing
    , const = Nothing
    , allOf = Nothing
    , anyOf = Nothing
    , oneOf = Nothing
    , not = Nothing
    }


type RowEncoder a
    = RowEncoder (Maybe a) String (a -> Value)


{-|
-}
encode : Schema -> Value
encode s =
    let
        optionally : (a -> Value) -> Maybe a -> String -> List ( String, Value ) -> List ( String, Value )
        optionally fn val key res =
            case val of
                Just s ->
                    ( key, fn s ) :: res

                Nothing ->
                    res

        encodeItems : Items -> List ( String, Value ) -> List ( String, Value )
        encodeItems items res =
            case items of
                ItemDefinition id ->
                    ( "items", encode id ) :: res

                ArrayOfItems aoi ->
                    ( "items", aoi |> List.map encode |> Encode.list ) :: res

                NoItems ->
                    res

        encodeDependency : Dependency -> Value
        encodeDependency dep =
            case dep of
                PropSchema ps ->
                    encode ps

                ArrayPropNames apn ->
                    apn |> List.map Encode.string |> Encode.list

        encodeDependencies : List ( String, Dependency ) -> List ( String, Value ) -> List ( String, Value )
        encodeDependencies deps res =
            if List.isEmpty deps then
                res
            else
                ( "dependencies", deps |> List.map (\( key, dep ) -> ( key, encodeDependency dep )) |> Encode.object ) :: res

        singleTypeToString : SingleType -> String
        singleTypeToString st =
            case st of
                StringType ->
                    "string"

                IntegerType ->
                    "integer"

                NumberType ->
                    "number"

                BooleanType ->
                    "boolean"

                ObjectType ->
                    "object"

                ArrayType ->
                    "array"

                NullType ->
                    "null"

        encodeType : Type -> List ( String, Value ) -> List ( String, Value )
        encodeType t res =
            case t of
                SingleType st ->
                    ( "type", st |> singleTypeToString |> Encode.string ) :: res

                NullableType st ->
                    ( "type", st |> singleTypeToString |> Encode.string ) :: res

                UnionType ut ->
                    ( "type", ut |> List.map (singleTypeToString >> Encode.string) |> Encode.list ) :: res

                AnyType ->
                    res

        encodeListSchemas : List Schema -> Value
        encodeListSchemas l =
            l
                |> List.map encode
                |> Encode.list

        encodeSchemata : Schemata -> Value
        encodeSchemata (Schemata l) =
            l
                |> List.map (\( s, x ) -> ( s, encode x ))
                |> Encode.object
    in
        case s of
            BooleanSchema bs ->
                Encode.bool bs

            ObjectSchema os ->
                [ encodeType os.type_
                , optionally Encode.string os.id "$id"
                , optionally Encode.string os.ref "$ref"
                , optionally Encode.string os.title "title"
                , optionally Encode.string os.description "description"
                , optionally identity os.default "default"
                , optionally Encode.list os.examples "examples"
                , optionally encodeSchemata os.definitions "definitions"
                , optionally Encode.float os.multipleOf "multipleOf"
                , optionally Encode.float os.maximum "maximum"
                , optionally Encode.float os.exclusiveMaximum "exclusiveMaximum"
                , optionally Encode.float os.minimum "minimum"
                , optionally Encode.float os.exclusiveMinimum "exclusiveMinimum"
                , optionally Encode.int os.maxLength "maxLength"
                , optionally Encode.int os.minLength "minLength"
                , optionally Encode.string os.pattern "pattern"
                , optionally Encode.string os.format "format"
                , encodeItems os.items
                , optionally encode os.additionalItems "additionalItems"
                , optionally Encode.int os.maxItems "maxItems"
                , optionally Encode.int os.minItems "minItems"
                , optionally Encode.bool os.uniqueItems "uniqueItems"
                , optionally encode os.contains "contains"
                , optionally Encode.int os.maxProperties "maxProperties"
                , optionally Encode.int os.minProperties "minProperties"
                , optionally (\s -> s |> List.map Encode.string |> Encode.list) os.required "required"
                , optionally encodeSchemata os.properties "properties"
                , optionally encodeSchemata os.patternProperties "patternProperties"
                , optionally encode os.additionalProperties "additionalProperties"
                , encodeDependencies os.dependencies
                , optionally encode os.propertyNames "propertyNames"
                , optionally Encode.list os.enum "enum"
                , optionally identity os.const "const"
                , optionally encodeListSchemas os.allOf "allOf"
                , optionally encodeListSchemas os.anyOf "anyOf"
                , optionally encodeListSchemas os.oneOf "oneOf"
                , optionally encode os.not "not"
                ]
                    |> List.foldl identity []
                    |> List.reverse
                    |> Encode.object


{-|
-}
decoder : Decoder Schema
decoder =
    let
        singleType =
            string
                |> andThen singleTypeDecoder

        multipleTypes =
            string
                |> list
                |> andThen multipleTypesDecoder

        booleanSchemaDecoder =
            Decode.bool
                |> Decode.andThen
                    (\b ->
                        if b then
                            succeed (BooleanSchema True)
                        else
                            succeed (BooleanSchema False)
                    )

        objectSchemaDecoder =
            decode SubSchema
                |> optional "type"
                    (Decode.oneOf [ multipleTypes, Decode.map SingleType singleType ])
                    AnyType
                |> optional "id" (nullable string) Nothing
                |> optional "$ref" (nullable string) Nothing
                -- meta
                |>
                    optional "title" (nullable string) Nothing
                |> optional "description" (nullable string) Nothing
                |> optional "default" (nullable value) Nothing
                |> optional "examples" (nullable <| list value) Nothing
                |> optional "definitions" (nullable <| lazy <| \_ -> schemataDecoder) Nothing
                -- number
                |>
                    optional "multipleOf" (nullable float) Nothing
                |> optional "maximum" (nullable float) Nothing
                |> optional "exclusiveMaximum" (nullable float) Nothing
                |> optional "minimum" (nullable float) Nothing
                |> optional "exclusiveMinimum" (nullable float) Nothing
                -- string
                |>
                    optional "maxLength" (nullable nonNegativeInt) Nothing
                |> optional "minLength" (nullable nonNegativeInt) Nothing
                |> optional "pattern" (nullable string) Nothing
                |> optional "format" (nullable string) Nothing
                -- array
                |>
                    optional "items" (lazy (\_ -> itemsDecoder)) NoItems
                |> optional "additionalItems" (nullable <| lazy (\_ -> decoder)) Nothing
                |> optional "maxItems" (nullable nonNegativeInt) Nothing
                |> optional "minItems" (nullable nonNegativeInt) Nothing
                |> optional "uniqueItems" (nullable bool) Nothing
                |> optional "contains" (nullable <| lazy (\_ -> decoder)) Nothing
                |> optional "maxProperties" (nullable nonNegativeInt) Nothing
                |> optional "minProperties" (nullable nonNegativeInt) Nothing
                |> optional "required" (nullable (list string)) Nothing
                |> optional "properties" (nullable (lazy (\_ -> schemataDecoder))) Nothing
                |> optional "patternProperties" (nullable (lazy (\_ -> schemataDecoder))) Nothing
                |> optional "additionalProperties" (nullable <| lazy (\_ -> decoder)) Nothing
                |> optional "dependencies" (lazy (\_ -> dependenciesDecoder)) []
                |> optional "propertyNames" (nullable <| lazy (\_ -> decoder)) Nothing
                |> optional "enum" (nullable nonEmptyUniqueArrayOfValuesDecoder) Nothing
                |> optional "const" (nullable value) Nothing
                |> optional "allOf" (nullable (lazy (\_ -> nonEmptyListOfSchemas))) Nothing
                |> optional "anyOf" (nullable (lazy (\_ -> nonEmptyListOfSchemas))) Nothing
                |> optional "oneOf" (nullable (lazy (\_ -> nonEmptyListOfSchemas))) Nothing
                |> optional "not" (nullable <| lazy (\_ -> decoder)) Nothing
    in
        Decode.oneOf
            [ booleanSchemaDecoder
            , objectSchemaDecoder
                |> Decode.andThen
                    (\b ->
                        succeed (ObjectSchema b)
                    )
            ]


nonEmptyListOfSchemas : Decoder (List Schema)
nonEmptyListOfSchemas =
    list (lazy (\_ -> decoder))
        |> andThen failIfEmpty


nonEmptyUniqueArrayOfValuesDecoder : Decoder (List Value)
nonEmptyUniqueArrayOfValuesDecoder =
    list value
        |> andThen failIfValuesAreNotUnique
        |> andThen failIfEmpty


failIfValuesAreNotUnique : List Value -> Decoder (List Value)
failIfValuesAreNotUnique l =
    succeed l


failIfEmpty : List a -> Decoder (List a)
failIfEmpty l =
    if List.isEmpty l then
        fail "List is empty"
    else
        succeed l


itemsDecoder : Decoder Items
itemsDecoder =
    Decode.oneOf
        [ Decode.map ArrayOfItems <| list decoder
        , Decode.map ItemDefinition decoder
        ]


dependenciesDecoder : Decoder (List ( String, Dependency ))
dependenciesDecoder =
    Decode.oneOf
        [ Decode.map ArrayPropNames (list string)
        , Decode.map PropSchema decoder
        ]
        |> Decode.keyValuePairs


nonNegativeInt : Decoder Int
nonNegativeInt =
    int
        |> andThen
            (\x ->
                if x >= 0 && isInt x then
                    succeed x
                else
                    fail "Expected non-negative int"
            )


{-|
Type property in json schema can be a single type or array of them, this type definition wraps up this complexity, also it introduces concept of nullable type, which is array of "null" type and a single type speaking JSON schema language, but also a useful concept to treat it separately from list of types.
-}
type Type
    = AnyType
    | SingleType SingleType
    | NullableType SingleType
    | UnionType (List SingleType)


{-|
-}
type SingleType
    = IntegerType
    | NumberType
    | StringType
    | BooleanType
    | ArrayType
    | ObjectType
    | NullType


multipleTypesDecoder : List String -> Decoder Type
multipleTypesDecoder lst =
    case lst of
        [ x, "null" ] ->
            Decode.map NullableType <| singleTypeDecoder x

        [ "null", x ] ->
            Decode.map NullableType <| singleTypeDecoder x

        [ x ] ->
            Decode.map SingleType <| singleTypeDecoder x

        otherList ->
            otherList
                |> List.sort
                |> List.map stringToType
                |> foldResults
                |> Result.andThen (Ok << UnionType)
                |> resultToDecoder


{-|
Attempt to parse string into a single type, it recognises the following list of types:
- integer
- number
- string
- boolean
- array
- object
- null
-}
stringToType : String -> Result String SingleType
stringToType s =
    case s of
        "integer" ->
            Ok IntegerType

        "number" ->
            Ok NumberType

        "string" ->
            Ok StringType

        "boolean" ->
            Ok BooleanType

        "array" ->
            Ok ArrayType

        "object" ->
            Ok ObjectType

        "null" ->
            Ok NullType

        _ ->
            Err ("Unknown type: " ++ s)


singleTypeDecoder : String -> Decoder SingleType
singleTypeDecoder s =
    case stringToType s of
        Ok st ->
            succeed st

        Err msg ->
            fail msg


schemataDecoder : Decoder Schemata
schemataDecoder =
    Decode.keyValuePairs (lazy (\_ -> decoder))
        |> Decode.andThen (\x -> succeed <| List.reverse x)
        |> Decode.map Schemata
