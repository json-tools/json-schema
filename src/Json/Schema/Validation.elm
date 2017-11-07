module Json.Schema.Validation exposing (Error, ValidationError(..), validate, JsonPath)

{-|

# Validate

@docs validate

# Validation Errors

@docs Error, ValidationError, JsonPath

-}

import Json.Decode as Decode exposing (Value, Decoder)
import Json.Encode as Encode exposing (int, float, string)
import Dict
import Regex
import Util exposing (isInt, isUnique)
import Ref exposing (resolveReference)
import Json.Schema.Definitions
    exposing
        ( Items(ItemDefinition, ArrayOfItems, NoItems)
        , Schemata(Schemata)
        , Dependency(ArrayPropNames, PropSchema)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        , Schema(ObjectSchema, BooleanSchema)
        , SubSchema
        , blankSchema
        , blankSubSchema
        )


{-|
-}
type alias JsonPath =
    List String


{-|
Attempt to validate returns `Result` with list of `Error` instances as an `Err`.
-}
type alias Error =
    { jsonPath : JsonPath
    , error : ValidationError
    }


{-|
Validation errors with details. The rule of parametrized errors like `Maximum` is that first parameter is always expected value, second parameter is actual value. Most of errors named after respective validation properties, only exception from this rule for cases like `AlwaysFail` which doesn't have keyword (this is result of boolean schema false), or `AdditionalPropertiesDisallowed` which represent subset of `.additionalProperties` validation when its value equals to `false` and additional property is present.

There are keywords in JSON Schema which doesn't have their dedicated error codes:

- items
- additionalItems
- properties
- patternProperties
- dependencies
- allOf
- oneOf

because the nature of these errors is to go deeper into the nested schema. Current implementation of validation only creates errors for leaves in Value, not for nodes, e.g. if one of properties fail validation, error list will contain error for property but not for the object containing it. This decision is made to reduce noise in errors, since it is kind of obvious that all parent objects containing invalid properties are also invalid, and this information can be derived from json path if needed.

-}
type ValidationError
    = MultipleOf Float Float
    | Maximum Float Float
    | Minimum Float Float
    | ExclusiveMaximum Float Float
    | ExclusiveMinimum Float Float
    | MaxLength Int Int
    | MinLength Int Int
    | Pattern String String
    | MaxItems Int Int
    | MinItems Int Int
    | UniqueItems
    | Contains
    | MaxProperties Int Int
    | MinProperties Int Int
    | Required (List String)
    | AdditionalPropertiesDisallowed
    | InvalidPropertyName (List Error)
    | Enum
    | Const
    | InvalidType String
    | AnyOf
    | OneOfNoneSucceed
    | OneOfManySucceed Int
    | Not
    | Ref
    | UnresolvableReference
    | AlwaysFail


{-| Validate value against schema
-}
validate : Value -> Schema -> Result (List Error) Value
validate value schema =
    let
        rootSchema =
            schema

        validateSubschema jsonPath os value =
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
            , validateNot
            , validateRef
            ]
                |> failWithListErrors jsonPath value os

        validateSchema jsonPath value s =
            case s of
                BooleanSchema bs ->
                    if bs then
                        Ok value
                    else
                        Err [ Error jsonPath AlwaysFail ]

                ObjectSchema os ->
                    validateSubschema jsonPath os value

        failWithListErrors : JsonPath -> Value -> SubSchema -> List (JsonPath -> Value -> SubSchema -> Result (List Error) Value) -> Result (List Error) Value
        failWithListErrors jsonPath value schema validators =
            validators
                |> List.foldl
                    (\fn ( errors, val ) ->
                        case fn jsonPath val schema of
                            Ok newValue ->
                                ( errors, newValue )

                            Err list ->
                                ( errors ++ list, val )
                    )
                    ( [], value )
                |> (\( errors, v ) ->
                        case errors of
                            [] ->
                                Ok v

                            list ->
                                Err list
                   )

        validateMultipleOf : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateMultipleOf jsonPath =
            when .multipleOf
                Decode.float
                (\multipleOf x ->
                    if isInt (x / multipleOf) then
                        Ok True
                    else
                        Err [ Error jsonPath <| MultipleOf multipleOf x ]
                )

        validateMaximum : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateMaximum jsonPath =
            when .maximum
                Decode.float
                (\max x ->
                    if x <= max then
                        Ok True
                    else
                        Err [ Error jsonPath <| Maximum max x ]
                )

        validateMinimum : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateMinimum jsonPath =
            when .minimum
                Decode.float
                (\min x ->
                    if x >= min then
                        Ok True
                    else
                        Err [ Error jsonPath <| Minimum min x ]
                )

        validateExclusiveMaximum : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateExclusiveMaximum jsonPath =
            when .exclusiveMaximum
                Decode.float
                (\max x ->
                    if x < max then
                        Ok True
                    else
                        Err [ Error jsonPath <| ExclusiveMaximum max x ]
                )

        validateExclusiveMinimum : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateExclusiveMinimum jsonPath =
            when .exclusiveMinimum
                Decode.float
                (\min x ->
                    if x > min then
                        Ok True
                    else
                        Err [ Error jsonPath <| ExclusiveMinimum min x ]
                )

        validateMaxLength : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateMaxLength jsonPath =
            when .maxLength
                Decode.string
                (\maxLength str ->
                    let
                        x =
                            String.length str
                    in
                        if x <= maxLength then
                            Ok True
                        else
                            Err [ Error jsonPath <| MaxLength maxLength x ]
                )

        validateMinLength : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateMinLength jsonPath =
            when .minLength
                Decode.string
                (\minLength str ->
                    let
                        x =
                            String.length str
                    in
                        if String.length str >= minLength then
                            Ok True
                        else
                            Err [ Error jsonPath <| MinLength minLength x ]
                )

        validatePattern : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validatePattern jsonPath =
            when .pattern
                Decode.string
                (\pattern str ->
                    if Regex.contains (Regex.regex pattern) str then
                        Ok True
                    else
                        Err [ Error jsonPath <| Pattern pattern str ]
                )

        validateItems : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateItems jsonPath value schema =
            let
                validateItem item schema index =
                    validateSchema (jsonPath ++ [ toString index ]) item schema
                        |> Result.map (\_ -> index + 1)
            in
                case schema.items of
                    ItemDefinition itemSchema ->
                        case Decode.decodeValue (Decode.list Decode.value) value of
                            Ok decoded ->
                                decoded
                                    |> List.foldl
                                        (\item res ->
                                            case res of
                                                Ok index ->
                                                    validateItem item itemSchema index

                                                _ ->
                                                    res
                                        )
                                        (Ok 0)
                                    |> Result.map (\_ -> value)

                            Err _ ->
                                Ok value

                    ArrayOfItems listItemSchemas ->
                        case Decode.decodeValue (Decode.list Decode.value) value of
                            Ok decoded ->
                                decoded
                                    |> List.foldl
                                        (\item res ->
                                            case res of
                                                Ok index ->
                                                    case List.drop index listItemSchemas |> List.head of
                                                        Just itemSchema ->
                                                            validateItem item itemSchema index

                                                        Nothing ->
                                                            case schema.additionalItems of
                                                                Just itemSchema ->
                                                                    validateItem item itemSchema index

                                                                Nothing ->
                                                                    Ok (index + 1)

                                                _ ->
                                                    res
                                        )
                                        (Ok 0)
                                    |> Result.map (\_ -> value)

                            Err _ ->
                                Ok value

                    _ ->
                        Ok value

        validateMaxItems : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateMaxItems jsonPath =
            when .maxItems
                (Decode.list Decode.value)
                (\maxItems list ->
                    let
                        x =
                            List.length list
                    in
                        if x <= maxItems then
                            Ok True
                        else
                            Err [ Error jsonPath <| MaxItems maxItems x ]
                )

        validateMinItems : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateMinItems jsonPath =
            when .minItems
                (Decode.list Decode.value)
                (\minItems list ->
                    let
                        x =
                            List.length list
                    in
                        if x >= minItems then
                            Ok True
                        else
                            Err [ Error jsonPath <| MinItems minItems x ]
                )

        validateUniqueItems : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateUniqueItems jsonPath =
            when .uniqueItems
                (Decode.list Decode.value)
                (\uniqueItems list ->
                    if not uniqueItems || isUniqueItems list then
                        Ok True
                    else
                        Err [ Error jsonPath UniqueItems ]
                )

        validateContains : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateContains jsonPath v =
            whenSubschema .contains
                (Decode.list Decode.value)
                (\contains list ->
                    if
                        List.any
                            (\item ->
                                case validateSchema jsonPath item contains of
                                    Ok _ ->
                                        True

                                    Err _ ->
                                        False
                            )
                            list
                    then
                        Ok v
                    else
                        Err [ Error jsonPath Contains ]
                )
                v

        validateMaxProperties : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateMaxProperties jsonPath =
            when .maxProperties
                (Decode.keyValuePairs Decode.value)
                (\maxProperties obj ->
                    let
                        x =
                            List.length obj
                    in
                        if x <= maxProperties then
                            Ok True
                        else
                            Err [ Error jsonPath <| MaxProperties maxProperties x ]
                )

        validateMinProperties : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateMinProperties jsonPath =
            when .minProperties
                (Decode.keyValuePairs Decode.value)
                (\minProperties obj ->
                    let
                        x =
                            List.length obj
                    in
                        if x >= minProperties then
                            Ok True
                        else
                            Err [ Error jsonPath <| MinProperties minProperties x ]
                )

        validateRequired : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateRequired jsonPath =
            when .required
                (Decode.keyValuePairs Decode.value)
                (\required obj ->
                    let
                        keys =
                            obj
                                |> List.map (\( key, _ ) -> key)

                        missing =
                            required
                                |> List.filter (flip List.member keys >> not)
                    in
                        if List.isEmpty missing then
                            Ok True
                        else
                            Err [ Error jsonPath <| Required missing ]
                )

        validateProperties : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateProperties jsonPath v =
            when .properties
                (Decode.keyValuePairs Decode.value)
                (\properties obj ->
                    obj
                        |> List.reverse
                        |> List.map
                            (\( key, value ) ->
                                case getSchema key properties of
                                    Just schema ->
                                        validateSchema (jsonPath ++ [ key ]) value schema

                                    Nothing ->
                                        Ok value
                            )
                        |> concatErrors (Ok v)
                )
                v

        validatePatternProperties : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validatePatternProperties jsonPath v =
            when .patternProperties
                (Decode.keyValuePairs Decode.value)
                (\(Schemata patternProperties) obj ->
                    List.foldl
                        (\( pattern, schema ) res ->
                            if res == (Ok v) then
                                obj
                                    |> getPropsByPattern pattern
                                    |> List.foldl
                                        (\( key, value ) res ->
                                            if res == (Ok v) then
                                                validateSchema (jsonPath ++ [ key ]) value schema
                                            else
                                                res
                                        )
                                        (Ok v)
                            else
                                res
                        )
                        (Ok v)
                        patternProperties
                )
                v

        validateAdditionalProperties : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateAdditionalProperties jsonPath v s =
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
                            |> (\obj ->
                                    case additionalProperties of
                                        BooleanSchema bs ->
                                            if bs then
                                                Ok v
                                            else
                                                Err [ Error jsonPath <| AdditionalPropertiesDisallowed ]

                                        ObjectSchema _ ->
                                            obj
                                                |> List.map
                                                    (\( key, val ) ->
                                                        validateSchema (jsonPath ++ [ key ]) val additionalProperties
                                                    )
                                                |> concatErrors (Ok v)
                               )
                    )
                    v
                    s

        validateDependencies : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateDependencies jsonPath v s =
            let
                validateDep obj =
                    s.dependencies
                        |> List.foldl
                            (\( depName, dep ) res ->
                                if res == (Ok v) && Dict.member depName (Dict.fromList obj) then
                                    case dep of
                                        PropSchema ss ->
                                            validateSchema jsonPath v ss

                                        ArrayPropNames keys ->
                                            validateSchema jsonPath v (ObjectSchema { blankSubSchema | required = Just keys })
                                else
                                    res
                            )
                            (Ok v)
            in
                if List.isEmpty s.dependencies then
                    Ok v
                else
                    case Decode.decodeValue (Decode.keyValuePairs Decode.value) v of
                        Ok v ->
                            validateDep v

                        Err _ ->
                            Ok v

        validatePropertyNames : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validatePropertyNames jsonPath v =
            let
                validatePropertyName schema key =
                    case validateSchema (jsonPath ++ [ key ]) (Encode.string key) schema of
                        Ok x ->
                            Nothing

                        Err list ->
                            Just list
            in
                whenSubschema
                    .propertyNames
                    (Decode.keyValuePairs Decode.value)
                    (\propertyNames obj ->
                        obj
                            |> List.map (\( key, _ ) -> key)
                            |> List.filterMap (validatePropertyName propertyNames)
                            |> (\invalidNames ->
                                    if List.isEmpty invalidNames then
                                        Ok v
                                    else
                                        Err [ Error jsonPath <| InvalidPropertyName <| List.concat invalidNames ]
                               )
                    )
                    v

        validateEnum : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateEnum jsonPath =
            when .enum
                Decode.value
                (\enum val ->
                    if List.any (\item -> stringify item == (stringify val)) enum then
                        Ok True
                    else
                        Err [ Error jsonPath Enum ]
                )

        validateConst : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateConst jsonPath =
            when .const
                Decode.value
                (\const val ->
                    let
                        expected =
                            toString const

                        actual =
                            toString val
                    in
                        if expected == actual then
                            Ok True
                        else
                            Err [ Error jsonPath Const ]
                )

        validateType : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateType jsonPath val s =
            case s.type_ of
                AnyType ->
                    Ok val

                SingleType st ->
                    validateSingleType jsonPath st val

                NullableType st ->
                    case validateSingleType jsonPath NullType val of
                        Err _ ->
                            validateSingleType jsonPath st val

                        _ ->
                            Ok val

                UnionType listTypes ->
                    if List.any (\st -> validateSingleType jsonPath st val == (Ok val)) listTypes then
                        Ok val
                    else
                        Err [ Error jsonPath <| InvalidType "None of desired types match" ]

        validateSingleType : JsonPath -> SingleType -> Value -> Result (List Error) Value
        validateSingleType jsonPath st val =
            let
                test : Decoder a -> Result (List Error) Value
                test d =
                    Decode.decodeValue d val
                        |> Result.map (\_ -> val)
                        |> Result.mapError (\s -> [ Error jsonPath <| InvalidType s ])
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

        validateAllOf : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateAllOf jsonPath =
            when .allOf
                Decode.value
                (\allOf val ->
                    List.foldl
                        (\schema res ->
                            if res == (Ok val) then
                                validateSchema jsonPath val schema
                            else
                                res
                        )
                        (Ok val)
                        allOf
                )

        validateAnyOf : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateAnyOf jsonPath =
            when .anyOf
                Decode.value
                (\anyOf val ->
                    let
                        validSubschema schema =
                            validateSchema jsonPath val schema == (Ok val)

                        isValid =
                            List.any validSubschema anyOf
                    in
                        if isValid then
                            Ok val
                        else
                            Err [ Error jsonPath AnyOf ]
                )

        validateOneOf : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateOneOf jsonPath =
            when .oneOf
                Decode.value
                (\oneOf val ->
                    let
                        validSubschema schema =
                            validateSchema jsonPath val schema == (Ok val)
                    in
                        case oneOf |> List.filter validSubschema |> List.length of
                            1 ->
                                Ok val

                            0 ->
                                Err [ Error jsonPath OneOfNoneSucceed ]

                            len ->
                                Err [ Error jsonPath <| OneOfManySucceed len ]
                )

        validateNot : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateNot jsonPath =
            whenSubschema .not
                Decode.value
                (\notSchema val ->
                    if validateSchema jsonPath val notSchema == (Ok val) then
                        Err [ Error jsonPath Not ]
                    else
                        Ok val
                )

        validateRef : JsonPath -> Value -> SubSchema -> Result (List Error) Value
        validateRef jsonPath =
            when .ref
                Decode.value
                (\ref val ->
                    ref
                        |> resolveReference rootSchema
                        |> Result.fromMaybe [ Error jsonPath UnresolvableReference ]
                        |> Result.andThen (validateSchema jsonPath val)
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
            list
                |> List.map toString
                |> isUnique

        when propOf decoder fn value schema =
            case propOf schema of
                Just v ->
                    case Decode.decodeValue decoder value of
                        Ok decoded ->
                            fn v decoded
                                |> Result.map (\_ -> value)

                        Err s ->
                            Ok value

                Nothing ->
                    Ok value

        whenSubschema propOf decoder fn value schema =
            case propOf schema of
                Just v ->
                    case Decode.decodeValue decoder value of
                        Ok decoded ->
                            fn v decoded
                                |> Result.map (\_ -> value)

                        Err s ->
                            Ok value

                Nothing ->
                    Ok value
    in
        validateSchema [] value schema


stringify : Value -> String
stringify =
    Encode.encode 0


concatErrors : Result (List b) a -> List (Result (List b) a) -> Result (List b) a
concatErrors =
    List.foldl
        (\x res ->
            case x of
                Ok _ ->
                    res

                Err list ->
                    case res of
                        Ok xx ->
                            x

                        Err list2 ->
                            Err (list2 ++ list)
        )
