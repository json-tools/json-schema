module Json.Schema
    exposing
        ( Schema
        , Properties
        , empty
        , fromValue
        , fromString
        , registerProperty
        , setValue
        , getValue
        , defaultFor
        , getString
        , getInt
        , getBool
        , getLength
        , mapProperties
        , validate
        )

{-| This library provides bunch of utility methods to work with JSON values using
schemas defined in json-schema format

# Definition
@docs Schema, Properties

# Schema creation
@docs empty, fromString, fromValue

# Schema manipulation
@docs registerProperty

# Reading value
@docs defaultFor, getBool, getInt, getLength, getString, getValue

# Writing value

@docs setValue

# Helpers
@docs mapProperties

-}

import Set
import Json.Decode.Extra as DecodeExtra exposing ((|:), withDefault)
import Json.Decode as Decode exposing (Decoder, maybe, string, bool, succeed, field, lazy)
import Json.Encode as Encode exposing (Value)
import Data.Schema
    exposing
        ( Items(ItemDefinition, ArrayOfItems, NoItems)
        , SubSchema(SubSchema, NoSchema)
        , Schemata(Schemata)
        , Dependency(ArrayPropNames, PropSchema)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        , blankSchema
        )
import Dict exposing (Dict)
import String
import Regex
import List.Extra


{-| Schema represents a node of schema.
Leaf types are `string`, `int`, and `boolean`.
Composite types are `array` and `object`.
Array item type defined in `items`.
Object properties defined in `properties`.
-}
type alias Schema =
    Data.Schema.Schema


{-| Properties - list of properties definitions for node with type = object
-}
type Properties
    = Properties (List ( String, Schema ))


{-| Empty schema object, handy as default or initial value
-}
empty : Schema
empty =
    Data.Schema.blankSchema


{-| Build schema from JSON string.

    "{\"properties\": {\"foo\": {\"type\": \"string\"}}"
        |> fromString
-}
fromString : String -> Result String Schema
fromString str =
    Decode.decodeString decodeSchema str
        |> Result.andThen convert


{-| Build schema from JSON value.

    Encode.object
        [ ( "properties", Encode.object
            [ ( "foo", Encode.object
                [ ( "type", Encode.string "string")
                ]
            ) ]
        ) ]
            |> fromValue
-}
fromValue : Value -> Result String Data.Schema.Schema
fromValue val =
    Decode.decodeValue Data.Schema.decoder val



--|> Result.andThen convert


decodeSchema : Decoder Schema
decodeSchema =
    Data.Schema.decoder
--    succeed Schema
--        |: (withDefault "" <| field "type" string)
--        |: (withDefault Set.empty <| field "required" (DecodeExtra.set string))
--        |: (maybe <| field "default" Decode.value)
--        |: withDefault "" (field "description" string)
--        |: (maybe <| field "format" string)
--        |: (maybe <| field "$ref" string)
--        |: (maybe <| field "enum" (Decode.list string))
--        |: (maybe <| field "items" (lazy (\_ -> Decode.map ArrayItemDefinition decodeSchema)))
--        |: (withDefault (Properties []) <| field "properties" (lazy (\_ -> decodeProperties)))
--        |: (withDefault (Properties []) <| field "definitions" (lazy (\_ -> decodeProperties)))


decodeProperties : Decoder Properties
decodeProperties =
    succeed Properties
        |: Decode.keyValuePairs (lazy (\_ -> decodeSchema))


traverse : Schema -> List String -> Maybe Schema
traverse schema path =
    case path of
        section :: name :: [] ->
            case section of
                "definitions" ->
                    getDefinition schema.definitions name

                _ ->
                    Nothing

        _ ->
            Nothing


convert : Schema -> Result String Schema
convert rootSchema =
    let
        digDefinition : String -> Schema -> Result String Schema
        digDefinition ref node =
            String.split "/" ref
                |> List.drop 1
                |> traverse rootSchema
                |> Result.fromMaybe ("Unable to find definition for " ++ ref)

        updateProperties node =
            let
                tryNext ( key, prop ) list =
                    case walk prop of
                        Ok p ->
                            Ok (( key, p ) :: list)

                        Err s ->
                            Err s

                newProps props =
                    List.foldl
                        (\item res -> res |> Result.andThen (tryNext item))
                        (Ok [])
                        props
            in
                node.properties
                    |> Maybe.map
                        (\(Schemata props) ->
                            case newProps props of
                                Ok p ->
                                    Ok
                                        { node | properties = Just (Schemata p) }

                                Err s ->
                                    Err s
                        )
                    |> Maybe.withDefault (Ok node)


        updateArrayItemDef node =
            case node.items of
                NoItems ->
                    Ok node

                -- TODO: handle me correctly
                ArrayOfItems _ ->
                    Ok node

                ItemDefinition def ->
                    case walk def of
                        Ok newDef ->
                            Ok { node | items = ItemDefinition newDef }

                        Err s ->
                            Err s

        clarifyType node =
            let
                checkEnum node =
                    case node.enum of
                        Nothing ->
                            checkItems node

                        Just _ ->
                            { node | type_ = SingleType StringType }

                checkItems node =
                    case node.items of
                        NoItems ->
                            checkProperties node node.properties

                        _ ->
                            { node | type_ = SingleType ArrayType }

                checkProperties node p =
                    if p == Nothing then
                        { node | type_ = AnyType }
                    else
                        { node | type_ = SingleType ObjectType }
            in
                if node.type_ == AnyType then
                    Ok (checkEnum node)
                else
                    Ok node

        walk : Schema -> Result String Schema
        walk node =
            (case node.ref of
                Just ref ->
                    digDefinition ref node

                Nothing ->
                    Ok node
            )
                |> Result.andThen updateProperties
                |> Result.andThen updateArrayItemDef
                |> Result.andThen clarifyType
    in
        walk rootSchema


{-| Set value of node

    Encode.object []
        |> setValue
            simpleSchema
            [ "foo" ] -- path in schema
            ( Encode.string "bar" ) -- value to set
        |> Expect.equal
            ( Ok ( object [ ( "foo", Encode.string "bar" ) ] ) )
-}
setValue : Schema -> List String -> Value -> Value -> Result String Value
setValue schema subPath finalValue dataNode =
    case subPath of
        [] ->
            let
                tryDecoding decoder =
                    case Decode.decodeValue decoder finalValue of
                        Ok _ ->
                            Ok finalValue

                        Err x ->
                            Err x
            in
                case schema.type_ of
                    SingleType IntegerType ->
                        tryDecoding Decode.int

                    SingleType StringType ->
                        tryDecoding Decode.string

                    SingleType BooleanType ->
                        tryDecoding Decode.bool

                    _ ->
                        Ok finalValue

        key :: tail ->
            case schema.type_ of
                SingleType ObjectType ->
                    let
                        nodeDict =
                            decodeDict dataNode

                        value =
                            nodeDict
                                |> Dict.get key
                                |> withDefaultFor schema

                        updatedValue prop =
                            setValue
                                prop
                                tail
                                finalValue
                                value
                    in
                        case getDefinition schema.properties key of
                            Just prop ->
                                case updatedValue prop of
                                    Ok val ->
                                        nodeDict
                                            |> Dict.insert key val
                                            |> encodeDict
                                            |> \v -> Ok v

                                    Err e ->
                                        Err e

                            Nothing ->
                                Err ("Key '" ++ key ++ "' not found")

                SingleType ArrayType ->
                    let
                        index =
                            String.toInt key |> Result.withDefault 0

                        nodeList =
                            decodeList dataNode
                    in
                        case schema.items of
                            NoItems ->
                                Err "No items definition"

                            -- TODO: handle me correctly
                            ArrayOfItems _ ->
                                Err "No items definition"

                            ItemDefinition prop ->
                                case getListItem index nodeList of
                                    Just oldItem ->
                                        case setValue prop tail finalValue oldItem of
                                            Ok newValue ->
                                                setListItem index newValue nodeList
                                                    |> Encode.list
                                                    |> \v -> Ok v

                                            Err e ->
                                                Err e

                                    Nothing ->
                                        nodeList
                                            ++ [ defaultFor prop |> setValue prop tail finalValue |> Result.withDefault (defaultFor prop) ]
                                            |> Encode.list
                                            |> \v -> Ok v

                _ ->
                    Ok finalValue


getListItem : Int -> List a -> Maybe a
getListItem index list =
    let
        ( _, result ) =
            List.foldl
                (\item ( i, result ) ->
                    if index == i then
                        ( i + 1, Just item )
                    else
                        ( i + 1, result )
                )
                ( 0, Nothing )
                list
    in
        result


setListItem : Int -> a -> List a -> List a
setListItem index a list =
    List.indexedMap
        (\i item ->
            if index == i then
                a
            else
                item
        )
        list


{-| Return int leaf
-}
getInt : Schema -> List String -> Value -> Int
getInt schema path value =
    getValue schema path value
        |> Decode.decodeValue Decode.int
        |> Result.withDefault 0


{-| Return boolean leaf
-}
getBool : Schema -> List String -> Value -> Bool
getBool schema path value =
    getValue schema path value
        |> Decode.decodeValue Decode.bool
        |> Result.withDefault False


{-| Return string leaf
-}
getString : Schema -> List String -> Value -> String
getString schema path value =
    getValue schema path value
        |> Decode.decodeValue string
        |> Result.withDefault ""


{-| Return length of array node
-}
getLength : Schema -> List String -> Value -> Int
getLength schema path value =
    getValue schema path value
        |> decodeList
        |> List.length


{-| Return default (initial) value for schema node. When default value for node
is not specified, then default calculated based on type:

  - "boolean" -> False
  - "integer" -> 0
  - "array" -> []
  - "object" -> {props listed in required}
-}
defaultFor : Schema -> Value
defaultFor schema =
    let
        defaultChildProp propName =
            ( propName
            , case getDefinition schema.properties propName of
                Just s ->
                    defaultFor s

                Nothing ->
                    Encode.null
            )

        objectFromRequiredProps =
            schema.required
                |> Maybe.map(List.map defaultChildProp)

        defaultDefault =
            case schema.type_ of
                SingleType ObjectType ->
                    case objectFromRequiredProps of
                        Just props ->
                            Encode.object props

                        Nothing ->
                            Encode.null

                SingleType ArrayType ->
                    Encode.list []

                SingleType IntegerType ->
                    Encode.int 0

                SingleType BooleanType ->
                    Encode.bool False

                SingleType NumberType ->
                    Encode.float 0

                SingleType NullType ->
                    Encode.null

                -- TODO: no need to assume anything, fixme
                -- "any", "string" (assume those are strings)
                _ ->
                    Encode.string ""
    in
        case schema.default of
            Just default ->
                default

            Nothing ->
                defaultDefault


encodeDict : Dict String Value -> Value
encodeDict dict =
    Encode.object (Dict.toList dict)


decodeDict : Value -> Dict String Value
decodeDict val =
    Decode.decodeValue (Decode.dict Decode.value) val
        |> Result.withDefault Dict.empty


decodeList : Value -> List Value
decodeList val =
    Decode.decodeValue (Decode.list Decode.value) val
        |> Result.withDefault []


getDefinition : Maybe Schemata -> String -> Maybe Schema
getDefinition defs name =
    defs
        |> Maybe.andThen
            (\(Schemata x) ->
                List.foldl
                    (\( key, prop ) result ->
                        if name == key then
                            Just prop
                        else
                            result
                    )
                    Nothing
                    x
            )


{-| getValue
-}
getValue : Schema -> List String -> Value -> Value
getValue schema path inputData =
    case path of
        [] ->
            inputData

        key :: tail ->
            case schema.type_ of
                SingleType ObjectType ->
                    -- TODO: lookup definition using "ref" (need root schema for that)
                    case getDefinition schema.properties key of
                        Just def ->
                            inputData
                                |> decodeDict
                                |> Dict.get key
                                |> Maybe.withDefault (defaultFor def)
                                |> getValue def tail

                        Nothing ->
                            defaultFor schema

                SingleType ArrayType ->
                    let
                        i =
                            --Debug.log "array index is"
                            String.toInt key |> Result.withDefault 0
                    in
                        case schema.items of
                            ItemDefinition def ->
                                inputData
                                    |> decodeList
                                    |> List.drop i
                                    |> List.head
                                    |> Maybe.withDefault (Encode.string "")
                                    |> getValue def tail

                            NoItems ->
                                defaultFor schema

                            -- TODO: handle me correctly
                            ArrayOfItems _ ->
                                defaultFor schema

                _ ->
                    inputData


withDefaultFor : Schema -> Maybe Value -> Value
withDefaultFor schema =
    Maybe.withDefault <| defaultFor schema


{-| Iterate through properties
-}
mapProperties : Properties -> (( String, Schema ) -> a) -> List a
mapProperties (Properties props) fn =
    List.map fn props


{-| Register property
-}
registerProperty : String -> Schema -> Schema -> Schema
registerProperty name prop schema =
    let
        hasProperty name list =
            List.foldl (\( key, _ ) res -> res || name == key) False list

        upgrade : Maybe Schemata -> Maybe Schemata
        upgrade props =
            props
                |> Maybe.map
                    (\(Schemata schemata) ->
                        if hasProperty name schemata then
                            List.map
                                (\( n, p ) ->
                                    if n == name then
                                        ( n, prop )
                                    else
                                        ( n, p )
                                )
                                schemata
                        else
                            ( name, prop ) :: schemata
                    )
                |> Maybe.map Schemata
    in
        { schema | properties = upgrade schema.properties }


{-| Validate value agains schema
-}
validate : Value -> Data.Schema.Schema -> Result String Bool
validate value schema =
    [ validateMultipleOf
    , validateMaximum
    , validateMinimum
    , validateExclusiveMaximum
    , validateExclusiveMinimum
    , validateMaxLength
    , validateMinLength
    , validatePattern
    , validateItems
    , validateMaxItems
    , validateMinItems
    , validateUniqueItems
    , validateContains
    , validateMaxProperties
    , validateMinProperties
    , validateRequired
    , validateProperties
    , validatePatternProperties
    , validateAdditionalProperties
    , validateDependencies
    , validatePropertyNames
    , validateEnum
    , validateConst
    , validateType
    , validateAllOf
    , validateAnyOf
    , validateOneOf
    ]
        |> failWithFirstError value schema


failWithFirstError : Value -> Data.Schema.Schema -> List (Value -> Data.Schema.Schema -> Result String Bool) -> Result String Bool
failWithFirstError value schema results =
    let
        failIfError fn prevResult =
            case prevResult of
                Err _ ->
                    prevResult

                _ ->
                    fn value schema
    in
        List.foldl failIfError (Ok True) results


validateMultipleOf : Value -> Data.Schema.Schema -> Result String Bool
validateMultipleOf =
    when .multipleOf
        Decode.float
        (\multipleOf x ->
            if isInt (x / multipleOf) then
                Ok True
            else
                Err <| "Value is not the multiple of " ++ (toString multipleOf)
        )


validateMaximum : Value -> Data.Schema.Schema -> Result String Bool
validateMaximum =
    when .maximum
        Decode.float
        (\max x ->
            if x <= max then
                Ok True
            else
                Err <| "Value is above the maximum of " ++ (toString max)
        )


validateMinimum : Value -> Data.Schema.Schema -> Result String Bool
validateMinimum =
    when .minimum
        Decode.float
        (\min x ->
            if x >= min then
                Ok True
            else
                Err <| "Value is below the minimum of " ++ (toString min)
        )


validateExclusiveMaximum : Value -> Data.Schema.Schema -> Result String Bool
validateExclusiveMaximum =
    when .exclusiveMaximum
        Decode.float
        (\max x ->
            if x < max then
                Ok True
            else
                Err <| "Value is not below the exclusive maximum of " ++ (toString max)
        )


validateExclusiveMinimum : Value -> Data.Schema.Schema -> Result String Bool
validateExclusiveMinimum =
    when .exclusiveMinimum
        Decode.float
        (\min x ->
            if x > min then
                Ok True
            else
                Err <| "Value is not above the exclusive minimum of " ++ (toString min)
        )


validateMaxLength : Value -> Data.Schema.Schema -> Result String Bool
validateMaxLength =
    when .maxLength
        Decode.string
        (\maxLength str ->
            if String.length str <= maxLength then
                Ok True
            else
                Err <| "String is longer than expected " ++ (toString maxLength)
        )


validateMinLength : Value -> Data.Schema.Schema -> Result String Bool
validateMinLength =
    when .minLength
        Decode.string
        (\minLength str ->
            if String.length str >= minLength then
                Ok True
            else
                Err <| "String is shorter than expected " ++ (toString minLength)
        )


validatePattern : Value -> Data.Schema.Schema -> Result String Bool
validatePattern =
    when .pattern
        Decode.string
        (\pattern str ->
            if Regex.contains (Regex.regex pattern) str then
                Ok True
            else
                Err <| "String does not match the regex pattern"
        )


validateItems : Value -> Data.Schema.Schema -> Result String Bool
validateItems value schema =
    let
        validateItem item schema index =
            validate item schema
                |> Result.mapError (\err -> "Item at index " ++ (toString index) ++ ": " ++ err)
                |> Result.map (\_ -> index + 1)
    in
        case schema.items of
            ItemDefinition itemSchema ->
                Decode.decodeValue (Decode.list Decode.value) value
                    |> Result.andThen
                        (List.foldl
                            (\item res ->
                                case res of
                                    Ok index ->
                                        validateItem item itemSchema index

                                    _ ->
                                        res
                            )
                         <|
                            Ok 0
                        )
                    |> Result.map (\_ -> True)

            ArrayOfItems listItemSchemas ->
                Decode.decodeValue (Decode.list Decode.value) value
                    |> Result.andThen
                        (List.foldl
                            (\item res ->
                                case res of
                                    Ok index ->
                                        case List.drop index listItemSchemas |> List.head of
                                            Just itemSchema ->
                                                validateItem item itemSchema index

                                            Nothing ->
                                                case schema.additionalItems of
                                                    SubSchema itemSchema ->
                                                        validateItem item itemSchema index

                                                    NoSchema ->
                                                        Ok (index + 1)

                                    _ ->
                                        res
                            )
                         <|
                            Ok 0
                        )
                    |> Result.map (\_ -> True)

            _ ->
                Ok True


validateMaxItems : Value -> Data.Schema.Schema -> Result String Bool
validateMaxItems =
    when .maxItems
        (Decode.list Decode.value)
        (\maxItems list ->
            if List.length list <= maxItems then
                Ok True
            else
                Err <| "Array has more items than expected (maxItems=" ++ (toString maxItems) ++ ")"
        )


validateMinItems : Value -> Data.Schema.Schema -> Result String Bool
validateMinItems =
    when .minItems
        (Decode.list Decode.value)
        (\minItems list ->
            if List.length list >= minItems then
                Ok True
            else
                Err <| "Array has less items than expected (minItems=" ++ (toString minItems) ++ ")"
        )


validateUniqueItems : Value -> Data.Schema.Schema -> Result String Bool
validateUniqueItems =
    when .uniqueItems
        (Decode.list Decode.value)
        (\uniqueItems list ->
            if not uniqueItems || isUniqueItems list then
                Ok True
            else
                Err <| "Array has not unique items"
        )


validateContains : Value -> Data.Schema.Schema -> Result String Bool
validateContains =
    whenSubschema .contains
        (Decode.list Decode.value)
        (\contains list ->
            if List.any (\item -> validate item contains == (Ok True)) list then
                Ok True
            else
                Err <| "Array does not contain expected value"
        )


validateMaxProperties : Value -> Data.Schema.Schema -> Result String Bool
validateMaxProperties =
    when .maxProperties
        (Decode.keyValuePairs Decode.value)
        (\maxProperties obj ->
            if List.length obj <= maxProperties then
                Ok True
            else
                Err <| "Object has more properties than expected (maxProperties=" ++ (toString maxProperties) ++ ")"
        )


validateMinProperties : Value -> Data.Schema.Schema -> Result String Bool
validateMinProperties =
    when .minProperties
        (Decode.keyValuePairs Decode.value)
        (\minProperties obj ->
            if List.length obj >= minProperties then
                Ok True
            else
                Err <| "Object has less properties than expected (minProperties=" ++ (toString minProperties) ++ ")"
        )


validateRequired : Value -> Data.Schema.Schema -> Result String Bool
validateRequired =
    when .required
        (Decode.keyValuePairs Decode.value)
        (\required obj ->
            let
                keys =
                    obj
                        |> List.map (\( key, _ ) -> key)
            in
                if required |> List.all (\a -> List.member a keys) then
                    Ok True
                else
                    Err <| "Object doesn't have all the required properties"
        )


validateProperties : Value -> Data.Schema.Schema -> Result String Bool
validateProperties =
    when .properties
        (Decode.keyValuePairs Decode.value)
        (\properties obj ->
            List.foldl
                (\( key, value ) res ->
                    if res == (Ok True) then
                        case getSchema key properties of
                            Just schema ->
                                validate value schema
                                    |> Result.mapError (\s -> "Invalid property '" ++ key ++ "': " ++ s)

                            Nothing ->
                                Ok True
                    else
                        res
                )
                (Ok True)
                obj
        )


validatePatternProperties : Value -> Data.Schema.Schema -> Result String Bool
validatePatternProperties =
    when .patternProperties
        (Decode.keyValuePairs Decode.value)
        (\(Schemata patternProperties) obj ->
            List.foldl
                (\( pattern, schema ) res ->
                    if res == (Ok True) then
                        obj
                            |> getPropsByPattern pattern
                            |> List.foldl
                                (\( key, value ) res ->
                                    if res == (Ok True) then
                                        validate value schema
                                            |> Result.mapError (\s -> "Invalid property '" ++ key ++ "': " ++ s)
                                    else
                                        res
                                )
                                (Ok True)
                    else
                        res
                )
                (Ok True)
                patternProperties
        )


validateAdditionalProperties : Value -> Data.Schema.Schema -> Result String Bool
validateAdditionalProperties v s =
    let
        rejectMatching :
            Maybe Schemata
            -> (String -> String -> Bool)
            -> List ( String, Value )
            -> List ( String, Value )
        rejectMatching props fn obj =
            case props of
                Just (Schemata p) ->
                    let
                        keys =
                            p |> List.map (\( k, _ ) -> k)
                    in
                        obj
                            |> List.filter (\( key, _ ) -> List.any (\pr -> fn pr key |> not) keys)

                Nothing ->
                    obj
    in
        whenSubschema .additionalProperties
            (Decode.keyValuePairs Decode.value)
            (\additionalProperties obj ->
                obj
                    |> rejectMatching s.properties (\a b -> a == b)
                    |> rejectMatching s.patternProperties (\a b -> Regex.contains (Regex.regex a) b)
                    |> List.foldl
                        (\( key, value ) res ->
                            if res == (Ok True) then
                                validate value additionalProperties
                                    |> Result.mapError (\s -> "Invalid property '" ++ key ++ "': " ++ s)
                            else
                                res
                        )
                        (Ok True)
            )
            v
            s


validateDependencies : Value -> Data.Schema.Schema -> Result String Bool
validateDependencies v s =
    let
        validateDep obj =
            s.dependencies
                |> List.foldl
                    (\( depName, dep ) res ->
                        if res == (Ok True) && Dict.member depName (Dict.fromList obj) then
                            case dep of
                                PropSchema ss ->
                                    validate v ss

                                ArrayPropNames keys ->
                                    validate v { blankSchema | required = Just keys }
                        else
                            res
                    )
                    (Ok True)
    in
        if List.isEmpty s.dependencies then
            Ok True
        else
            v
                |> Decode.decodeValue (Decode.keyValuePairs Decode.value)
                |> Result.andThen validateDep


validatePropertyNames : Value -> Data.Schema.Schema -> Result String Bool
validatePropertyNames =
    whenSubschema
        .propertyNames
        (Decode.keyValuePairs Decode.value)
        (\propertyNames obj ->
            List.foldl
                (\( key, _ ) res ->
                    if res == (Ok True) then
                        validate (Encode.string key) propertyNames
                            |> Result.mapError (\e -> "Property '" ++ key ++ "' doesn't validate against peopertyNames schema: " ++ e)
                    else
                        Ok True
                )
                (Ok True)
                obj
        )


validateEnum : Value -> Data.Schema.Schema -> Result String Bool
validateEnum =
    when .enum
        Decode.value
        (\enum val ->
            if List.any (\item -> toString item == (toString val)) enum then
                Ok True
            else
                Err "Value is not present in enum"
        )


validateConst : Value -> Data.Schema.Schema -> Result String Bool
validateConst =
    when .const
        Decode.value
        (\const val ->
            if toString const == (toString val) then
                Ok True
            else
                Err "Value doesn't equal const"
        )


validateType : Value -> Data.Schema.Schema -> Result String Bool
validateType val s =
    case s.type_ of
        AnyType ->
            Ok True

        SingleType st ->
            validateSingleType st val

        NullableType st ->
            case validateSingleType NullType val of
                Err _ ->
                    validateSingleType st val

                _ ->
                    Ok True

        UnionType listTypes ->
            if List.any (\st -> validateSingleType st val == (Ok True)) listTypes then
                Ok True
            else
                Err "Type mismatch"



-- Type(AnyType, SingleType, NullableType, UnionType)
-- SingleType(IntegerType, NumberType, StringType, NullType, ArrayType, ObjectType)


validateSingleType : SingleType -> Value -> Result String Bool
validateSingleType st val =
    let
        test : Decoder a -> Result String Bool
        test d =
            Decode.decodeValue d val
                |> Result.map (\_ -> True)
    in
        case st of
            IntegerType ->
                test Decode.int

            NumberType ->
                test Decode.float

            StringType ->
                test Decode.string

            BooleanType ->
                test Decode.bool

            NullType ->
                test <| Decode.null Nothing

            ArrayType ->
                test <| Decode.list Decode.value

            ObjectType ->
                test <| Decode.keyValuePairs Decode.value


validateAllOf : Value -> Data.Schema.Schema -> Result String Bool
validateAllOf =
    when .allOf
        Decode.value
        (\allOf val ->
            List.foldl
                (\subschema res ->
                    if res == (Ok True) then
                        case subschema of
                            SubSchema schema ->
                                validate val schema

                            NoSchema ->
                                Ok True
                    else
                        res
                )
                (Ok True)
                allOf
        )


validateAnyOf : Value -> Data.Schema.Schema -> Result String Bool
validateAnyOf =
    when .anyOf
        Decode.value
        (\anyOf val ->
            let
                validSubschema subschema =
                    case subschema of
                        SubSchema schema ->
                            validate val schema == (Ok True)

                        NoSchema ->
                            True

                isValid =
                    List.any validSubschema anyOf
            in
                if isValid then
                    Ok True
                else
                    Err "None of the schemas in anyOf allow this value"
        )


validateOneOf : Value -> Data.Schema.Schema -> Result String Bool
validateOneOf =
    when .oneOf
        Decode.value
        (\oneOf val ->
            let
                validSubschema subschema =
                    case subschema of
                        SubSchema schema ->
                            validate val schema == (Ok True)

                        NoSchema ->
                            True

                validSchemas =
                    List.filter validSubschema oneOf

                numValid =
                    List.length validSchemas
            in
                if numValid == 1 then
                    Ok True
                else if numValid == 0 then
                    Err "None of the schemas in anyOf allow this value"
                else
                    Err <| "oneOf expects value to succeed validation against exactly one schema but " ++ (toString numValid) ++ " validations succeeded"
        )


getSchema key (Schemata props) =
    props
        |> List.filter (\( k, _ ) -> k == key)
        |> List.map (\( _, s ) -> s)
        |> List.head


getPropsByPattern pattern props =
    props
        |> List.filter (\( k, _ ) -> Regex.contains (Regex.regex pattern) k)


isUniqueItems list =
    let
        strings =
            List.map toString list

        originalLength =
            List.length list
    in
        strings
            |> List.Extra.unique
            |> List.length
            |> (==) originalLength


isInt : Float -> Bool
isInt x =
    x == (round >> toFloat) x


when propOf decoder fn value schema =
    case propOf schema of
        Just v ->
            Decode.decodeValue decoder value
                |> Result.andThen (fn v)

        Nothing ->
            Ok True


whenSubschema propOf decoder fn value schema =
    case propOf schema of
        SubSchema v ->
            Decode.decodeValue decoder value
                |> Result.andThen (fn v)

        NoSchema ->
            Ok True
