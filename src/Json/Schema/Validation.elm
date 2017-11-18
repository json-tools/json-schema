module Json.Schema.Validation exposing (Error, ValidationError(..), validate, JsonPointer)

{-|

# Validate

Validation fails with list of errors, one for each invalid leaf of the value object.
When validation succeeds it also returns value being validated. Currently this value is the same as initial value, later version will allow options to be supplied in order to normalize value along the validation (e.g. apply defaults, remove additional properties, coerce types)

@docs validate

# Validation Errors

@docs Error, ValidationError, JsonPointer

-}

import Json.Decode as Decode exposing (Value, Decoder)
import Json.Encode as Encode exposing (int, float, string)
import String.UTF32 as UTF32
import Dict
import Regex
import Util exposing (isInt, indexOfFirstDuplicate)
import Ref exposing (resolveReference, SchemataPool)
import Json.Schemata as Schemata
import Json.Schema.Definitions
    exposing
        ( Items(ItemDefinition, ArrayOfItems, NoItems)
        , Schemata(Schemata)
        , Dependency(ArrayPropNames, PropSchema)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        , Schema(ObjectSchema, BooleanSchema)
        , ExclusiveBoundary(BoolBoundary, NumberBoundary)
        , SubSchema
        , blankSchema
        , blankSubSchema
        , decoder
        )


{-|
Path in json value.

A few notes:

- empty list represents root
- indices of array items are integers encoded as string

-}
type alias JsonPointer =
    { ns : String
    , path : List String
    }


{-|
Attempt to validate returns `Result` with list of `Error` instances as an `Err`.
-}
type alias Error =
    { jsonPointer : JsonPointer
    , details : ValidationError
    }


{-|
Validation errors with details. The rule of parametrized errors like `Maximum` is that first parameter is always expected value, second parameter is actual value. Most of errors named after respective validation properties, only exception from this rule for cases like `AlwaysFail` which doesn't have keyword (this is result of boolean schema false), or `AdditionalPropertiesDisallowed` which represent subset of `.additionalProperties` validation when its value equals to `false` and additional property is present.

There are keywords in JSON Schema which don't have their dedicated error codes:

- items
- additionalItems
- properties
- patternProperties
- dependencies
- allOf
- oneOf

The reason for this is the nature of these errors is to go deeper into the nested Schema and Value.

Current implementation of validation only creates errors for leaves of the Value, not for nodes, e.g. if one of the properties fails a validation, error list will contain an error for the property but not for the object containing it. This decision is made to reduce noise in errors, since it is obvious that all the parent objects containing invalid properties are also invalid, and this information can be derived from json path if needed.

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
    | UniqueItems Value
    | Contains
    | MaxProperties Int Int
    | MinProperties Int Int
    | Required (List String)
    | AdditionalPropertiesDisallowed (List String)
    | InvalidPropertyName (List Error)
    | Enum
    | Const
    | InvalidType String
    | OneOfNoneSucceed
    | OneOfManySucceed Int
    | Not
    | UnresolvableReference String
    | AlwaysFail


{-| Validate value against schema
-}
validate : SchemataPool -> Value -> Schema -> Schema -> Result (List Error) Value
validate pool value rootSchema schema =
    let
        validateSubschema jsonPointer os value =
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
            ]
                |> failWithListErrors jsonPointer value os

        validateSchema jsonPointer value s =
            case s of
                BooleanSchema bs ->
                    if bs then
                        Ok value
                    else
                        Err [ Error jsonPointer AlwaysFail ]

                ObjectSchema os ->
                    case os.ref of
                        Just ref ->
                            case ref |> resolveReference jsonPointer.ns pool rootSchema of
                                Just ( ns, ObjectSchema oss ) ->
                                    validateSubschema { jsonPointer | ns = ns } oss value

                                Just ( ns, BooleanSchema bs ) ->
                                    if bs then
                                        Ok value
                                    else
                                        Err [ Error jsonPointer AlwaysFail ]

                                Nothing ->
                                    Err [ Error jsonPointer <| UnresolvableReference ref ]

                        Nothing ->
                            validateSubschema jsonPointer os value

        failWithListErrors : JsonPointer -> Value -> SubSchema -> List (JsonPointer -> Value -> SubSchema -> Result (List Error) Value) -> Result (List Error) Value
        failWithListErrors jsonPointer value schema validators =
            validators
                |> List.foldl
                    (\fn ( errors, val ) ->
                        case fn jsonPointer val schema of
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

        validateMultipleOf : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMultipleOf jsonPointer =
            when .multipleOf
                Decode.float
                (\multipleOf x ->
                    if isInt (x / multipleOf) then
                        Ok True
                    else
                        Err [ Error jsonPointer <| MultipleOf multipleOf x ]
                )

        validateMaximum : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMaximum jsonPointer v s =
            when .maximum
                Decode.float
                (\max x ->
                    case s.exclusiveMaximum of
                        Just (BoolBoundary True) ->
                            if x < max then
                                Ok True
                            else
                                Err [ Error jsonPointer <| ExclusiveMaximum max x ]

                        _ ->
                            if x <= max then
                                Ok True
                            else
                                Err [ Error jsonPointer <| Maximum max x ]
                )
                v
                s

        validateMinimum : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMinimum jsonPointer v s =
            when .minimum
                Decode.float
                (\min x ->
                    case s.exclusiveMinimum of
                        Just (BoolBoundary True) ->
                            if x > min then
                                Ok True
                            else
                                Err [ Error jsonPointer <| ExclusiveMinimum min x ]

                        _ ->
                            if x >= min then
                                Ok True
                            else
                                Err [ Error jsonPointer <| Minimum min x ]
                )
                v
                s

        validateExclusiveMaximum : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateExclusiveMaximum jsonPointer v s =
            when .exclusiveMaximum
                Decode.float
                (\max x ->
                    case max of
                        NumberBoundary m ->
                            if x < m then
                                Ok True
                            else
                                Err [ Error jsonPointer <| ExclusiveMaximum m x ]

                        BoolBoundary _ ->
                            -- draft-04 exclusive boundary validation only works as part of minimum/maximum
                            Ok True
                )
                v
                s

        validateExclusiveMinimum : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateExclusiveMinimum jsonPointer v s =
            when .exclusiveMinimum
                Decode.float
                (\min x ->
                    case min of
                        NumberBoundary m ->
                            if x > m then
                                Ok True
                            else
                                Err [ Error jsonPointer <| ExclusiveMinimum m x ]

                        BoolBoundary _ ->
                            -- draft-04 exclusive boundary validation only works as part of minimum/maximum
                            Ok True
                )
                v
                s

        validateMaxLength : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMaxLength jsonPointer =
            when .maxLength
                Decode.string
                (\maxLength str ->
                    let
                        x =
                            UTF32.length str
                    in
                        if x <= maxLength then
                            Ok True
                        else
                            Err [ Error jsonPointer <| MaxLength maxLength x ]
                )

        validateMinLength : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMinLength jsonPointer =
            when .minLength
                Decode.string
                (\minLength str ->
                    let
                        x =
                            UTF32.length str
                    in
                        if x >= minLength then
                            Ok True
                        else
                            Err [ Error jsonPointer <| MinLength minLength x ]
                )

        validatePattern : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validatePattern jsonPointer =
            when .pattern
                Decode.string
                (\pattern str ->
                    if Regex.contains (Regex.regex pattern) str then
                        Ok True
                    else
                        Err [ Error jsonPointer <| Pattern pattern str ]
                )

        validateItems : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateItems jsonPointer value schema =
            let
                validateItem item schema index =
                    validateSchema ({ jsonPointer | path = jsonPointer.path ++ [ toString index ] }) item schema
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

        validateMaxItems : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMaxItems jsonPointer =
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
                            Err [ Error jsonPointer <| MaxItems maxItems x ]
                )

        validateMinItems : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMinItems jsonPointer =
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
                            Err [ Error jsonPointer <| MinItems minItems x ]
                )

        validateUniqueItems : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateUniqueItems jsonPointer =
            when .uniqueItems
                (Decode.list Decode.value)
                (\uniqueItems list ->
                    if not uniqueItems then
                        Ok True
                    else
                        case findDuplicateItem list of
                            Just v ->
                                Err [ Error jsonPointer <| UniqueItems v ]

                            Nothing ->
                                Ok True
                )

        validateContains : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateContains jsonPointer v =
            whenSubschema .contains
                (Decode.list Decode.value)
                (\contains list ->
                    if
                        List.any
                            (\item ->
                                case validateSchema jsonPointer item contains of
                                    Ok _ ->
                                        True

                                    Err _ ->
                                        False
                            )
                            list
                    then
                        Ok v
                    else
                        Err [ Error jsonPointer Contains ]
                )
                v

        validateMaxProperties : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMaxProperties jsonPointer =
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
                            Err [ Error jsonPointer <| MaxProperties maxProperties x ]
                )

        validateMinProperties : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMinProperties jsonPointer =
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
                            Err [ Error jsonPointer <| MinProperties minProperties x ]
                )

        validateRequired : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateRequired jsonPointer =
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
                            Err [ Error jsonPointer <| Required missing ]
                )

        validateProperties : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateProperties jsonPointer v =
            when .properties
                (Decode.keyValuePairs Decode.value)
                (\properties obj ->
                    obj
                        |> List.reverse
                        |> List.map
                            (\( key, value ) ->
                                case getSchema key properties of
                                    Just schema ->
                                        validateSchema { jsonPointer | path = jsonPointer.path ++ [ key ] } value schema

                                    Nothing ->
                                        Ok value
                            )
                        |> concatErrors (Ok v)
                )
                v

        {-
           Validation succeeds if, for each instance name that matches any regular
           expressions that appear as a property name in this keyword's value, the
           child instance for that name successfully validates against each schema
           that corresponds to a matching regular expression.

           Excerpt from http://json-schema.org/latest/json-schema-validation.html#rfc.section.6.19
        -}
        validatePatternProperties : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validatePatternProperties jsonPointer v =
            when .patternProperties
                (Decode.keyValuePairs Decode.value)
                (\(Schemata patternProperties) obj ->
                    List.foldl
                        (\( pattern, schema ) res ->
                            case res of
                                Ok _ ->
                                    obj
                                        |> getPropsByPattern pattern
                                        |> List.foldl
                                            (\( key, value ) res ->
                                                case res of
                                                    Ok _ ->
                                                        validateSchema { jsonPointer | path = jsonPointer.path ++ [ key ] } value schema

                                                    Err _ ->
                                                        res
                                            )
                                            (Ok v)

                                Err _ ->
                                    res
                        )
                        (Ok v)
                        patternProperties
                )
                v

        validateAdditionalProperties : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateAdditionalProperties jsonPointer v s =
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
                                whitelist =
                                    p |> List.map (\( k, _ ) -> k)
                            in
                                obj
                                    |> List.filter (\( key, _ ) -> whitelist |> List.any (\allowed -> fn allowed key) |> not)

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
                                            else if List.isEmpty obj then
                                                Ok v
                                            else
                                                Err [ Error jsonPointer <| AdditionalPropertiesDisallowed <| List.map (\( name, _ ) -> name) obj ]

                                        ObjectSchema _ ->
                                            obj
                                                |> List.map
                                                    (\( key, val ) ->
                                                        validateSchema { jsonPointer | path = jsonPointer.path ++ [ key ] } val additionalProperties
                                                    )
                                                |> concatErrors (Ok v)
                               )
                    )
                    v
                    s

        validateDependencies : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateDependencies jsonPointer v s =
            let
                validateDep obj =
                    s.dependencies
                        |> List.foldl
                            (\( depName, dep ) res ->
                                if res == (Ok v) && Dict.member depName (Dict.fromList obj) then
                                    case dep of
                                        PropSchema ss ->
                                            validateSchema jsonPointer v ss

                                        ArrayPropNames keys ->
                                            validateSchema jsonPointer v (ObjectSchema { blankSubSchema | required = Just keys })
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

        validatePropertyNames : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validatePropertyNames jsonPointer v =
            let
                validatePropertyName schema key =
                    case validateSchema { jsonPointer | path = jsonPointer.path ++ [ key ] } (Encode.string key) schema of
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
                                        Err [ Error jsonPointer <| InvalidPropertyName <| List.concat invalidNames ]
                               )
                    )
                    v

        validateEnum : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateEnum jsonPointer =
            when .enum
                Decode.value
                (\enum val ->
                    if List.any (\item -> stringify item == (stringify val)) enum then
                        Ok True
                    else
                        Err [ Error jsonPointer Enum ]
                )

        validateConst : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateConst jsonPointer =
            when .const
                Decode.value
                (\const val ->
                    let
                        expected =
                            canonical const

                        actual =
                            canonical val
                    in
                        if expected == actual then
                            Ok True
                        else
                            Err [ Error jsonPointer Const ]
                )

        validateType : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateType jsonPointer val s =
            case s.type_ of
                AnyType ->
                    Ok val

                SingleType st ->
                    validateSingleType jsonPointer st val

                NullableType st ->
                    case validateSingleType jsonPointer NullType val of
                        Err _ ->
                            validateSingleType jsonPointer st val

                        _ ->
                            Ok val

                UnionType listTypes ->
                    if List.any (\st -> validateSingleType jsonPointer st val == (Ok val)) listTypes then
                        Ok val
                    else
                        Err [ Error jsonPointer <| InvalidType "None of desired types match" ]

        validateSingleType : JsonPointer -> SingleType -> Value -> Result (List Error) Value
        validateSingleType jsonPointer st val =
            let
                test : Decoder a -> Result (List Error) Value
                test d =
                    Decode.decodeValue d val
                        |> Result.map (\_ -> val)
                        |> Result.mapError (\s -> [ Error jsonPointer <| InvalidType s ])
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

        validateAllOf : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateAllOf jsonPointer =
            when .allOf
                Decode.value
                (\allOf val ->
                    List.foldl
                        (\schema res ->
                            if res == (Ok val) then
                                validateSchema jsonPointer val schema
                            else
                                res
                        )
                        (Ok val)
                        allOf
                )

        validateAnyOf : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateAnyOf jsonPointer =
            when .anyOf
                Decode.value
                (\anyOf val ->
                    let
                        validationResults =
                            anyOf
                                |> List.map (validateSchema jsonPointer val)

                        isOk res =
                            case res of
                                Ok _ ->
                                    True

                                _ ->
                                    False
                    in
                        if List.any isOk validationResults then
                            Ok val
                        else
                            validationResults
                                |> concatErrors (Ok val)
                )

        validateOneOf : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateOneOf jsonPointer =
            when .oneOf
                Decode.value
                (\oneOf val ->
                    let
                        validSubschema schema =
                            validateSchema jsonPointer val schema == (Ok val)
                    in
                        case oneOf |> List.filter validSubschema |> List.length of
                            1 ->
                                Ok val

                            0 ->
                                Err [ Error jsonPointer OneOfNoneSucceed ]

                            len ->
                                Err [ Error jsonPointer <| OneOfManySucceed len ]
                )

        validateNot : JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateNot jsonPointer =
            whenSubschema .not
                Decode.value
                (\notSchema val ->
                    if validateSchema jsonPointer val notSchema == (Ok val) then
                        Err [ Error jsonPointer Not ]
                    else
                        Ok val
                )

        getSchema key (Schemata props) =
            props
                |> List.filter (\( k, _ ) -> k == key)
                |> List.map (\( _, s ) -> s)
                |> List.head

        getPropsByPattern pattern props =
            props
                |> List.filter (\( k, _ ) -> Regex.contains (Regex.regex pattern) k)

        findDuplicateItem list =
            list
                |> List.map (Encode.encode 0)
                |> indexOfFirstDuplicate
                |> (\x ->
                        if x == -1 then
                            Nothing
                        else
                            list
                                |> List.drop x
                                |> List.head
                   )

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
        validateSchema (JsonPointer "" []) value schema


stringify : Value -> String
stringify =
    Encode.encode 0


canonical : Value -> String
canonical v =
    case Decode.decodeValue (Decode.keyValuePairs Decode.value) v of
        Ok obj ->
            obj
                |> List.sortBy (\( k, _ ) -> k)
                |> Encode.object
                |> stringify

        Err _ ->
            stringify v


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
