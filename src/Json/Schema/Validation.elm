module Json.Schema.Validation exposing (Error, ValidationError(..), ValidationOptions, defaultOptions, validate, JsonPointer)

{-|

# Validate

Validation fails with list of errors, one for each invalid leaf of the value object.
When validation succeeds it also returns value being validated. Currently this value is the same as initial value, later version will allow options to be supplied in order to normalize value along the validation (e.g. apply defaults, remove additional properties, coerce types)

@docs validate, ValidationOptions, defaultOptions

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
Validation options which allow to apply defaults (more options upcoming)
-}
type alias ValidationOptions =
    { applyDefaults : Bool
    }


{-|
Default validation options, applyDefaults = True
-}
defaultOptions : ValidationOptions
defaultOptions =
    { applyDefaults = True
    }


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
validate : ValidationOptions -> SchemataPool -> Value -> Schema -> Schema -> Result (List Error) Value
validate validationOptions pool value rootSchema schema =
    let
        validateSubschema validationOptions jsonPointer os value =
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
            , validateProperties
            , validateMaxProperties
            , validateMinProperties
            , validateRequired
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
                |> failWithListErrors validationOptions jsonPointer value os

        validateSchema validationOptions jsonPointer value s =
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
                                    validateSubschema validationOptions { jsonPointer | ns = ns } oss value

                                Just ( ns, BooleanSchema bs ) ->
                                    if bs then
                                        Ok value
                                    else
                                        Err [ Error jsonPointer AlwaysFail ]

                                Nothing ->
                                    Err [ Error jsonPointer <| UnresolvableReference ref ]

                        Nothing ->
                            validateSubschema validationOptions jsonPointer os value

        failWithListErrors : ValidationOptions -> JsonPointer -> Value -> SubSchema -> List (ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value) -> Result (List Error) Value
        failWithListErrors validationOptions jsonPointer value schema validators =
            validators
                |> List.foldl
                    (\fn ( errors, val ) ->
                        case fn validationOptions jsonPointer val schema of
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

        validateMultipleOf : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMultipleOf validationOptions jsonPointer v =
            when .multipleOf
                Decode.float
                (\multipleOf x ->
                    if isInt (x / multipleOf) then
                        Ok v
                    else
                        Err [ Error jsonPointer <| MultipleOf multipleOf x ]
                )
                v

        validateMaximum : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMaximum validationOptions jsonPointer v s =
            when .maximum
                Decode.float
                (\max x ->
                    case s.exclusiveMaximum of
                        Just (BoolBoundary True) ->
                            if x < max then
                                Ok v
                            else
                                Err [ Error jsonPointer <| ExclusiveMaximum max x ]

                        _ ->
                            if x <= max then
                                Ok v
                            else
                                Err [ Error jsonPointer <| Maximum max x ]
                )
                v
                s

        validateMinimum : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMinimum validationOptions jsonPointer v s =
            when .minimum
                Decode.float
                (\min x ->
                    case s.exclusiveMinimum of
                        Just (BoolBoundary True) ->
                            if x > min then
                                Ok v
                            else
                                Err [ Error jsonPointer <| ExclusiveMinimum min x ]

                        _ ->
                            if x >= min then
                                Ok v
                            else
                                Err [ Error jsonPointer <| Minimum min x ]
                )
                v
                s

        validateExclusiveMaximum : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateExclusiveMaximum validationOptions jsonPointer v s =
            when .exclusiveMaximum
                Decode.float
                (\max x ->
                    case max of
                        NumberBoundary m ->
                            if x < m then
                                Ok v
                            else
                                Err [ Error jsonPointer <| ExclusiveMaximum m x ]

                        BoolBoundary _ ->
                            -- draft-04 exclusive boundary validation only works as part of minimum/maximum
                            Ok v
                )
                v
                s

        validateExclusiveMinimum : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateExclusiveMinimum validationOptions jsonPointer v s =
            when .exclusiveMinimum
                Decode.float
                (\min x ->
                    case min of
                        NumberBoundary m ->
                            if x > m then
                                Ok v
                            else
                                Err [ Error jsonPointer <| ExclusiveMinimum m x ]

                        BoolBoundary _ ->
                            -- draft-04 exclusive boundary validation only works as part of minimum/maximum
                            Ok v
                )
                v
                s

        validateMaxLength : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMaxLength validationOptions jsonPointer v =
            when .maxLength
                Decode.string
                (\maxLength str ->
                    let
                        x =
                            UTF32.length str
                    in
                        if x <= maxLength then
                            Ok v
                        else
                            Err [ Error jsonPointer <| MaxLength maxLength x ]
                )
                v

        validateMinLength : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMinLength validationOptions jsonPointer v =
            when .minLength
                Decode.string
                (\minLength str ->
                    let
                        x =
                            UTF32.length str
                    in
                        if x >= minLength then
                            Ok v
                        else
                            Err [ Error jsonPointer <| MinLength minLength x ]
                )
                v

        validatePattern : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validatePattern validationOptions jsonPointer v =
            when .pattern
                Decode.string
                (\pattern str ->
                    if Regex.contains (Regex.regex pattern) str then
                        Ok v
                    else
                        Err [ Error jsonPointer <| Pattern pattern str ]
                )
                v

        validateItems : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateItems validationOptions jsonPointer value schema =
            let
                validateItem item schema index =
                    validateSchema validationOptions ({ jsonPointer | path = jsonPointer.path ++ [ toString index ] }) item schema
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

        validateMaxItems : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMaxItems validationOptions jsonPointer v =
            when .maxItems
                (Decode.list Decode.value)
                (\maxItems list ->
                    let
                        x =
                            List.length list
                    in
                        if x <= maxItems then
                            Ok v
                        else
                            Err [ Error jsonPointer <| MaxItems maxItems x ]
                )
                v

        validateMinItems : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMinItems validationOptions jsonPointer v =
            when .minItems
                (Decode.list Decode.value)
                (\minItems list ->
                    let
                        x =
                            List.length list
                    in
                        if x >= minItems then
                            Ok v
                        else
                            Err [ Error jsonPointer <| MinItems minItems x ]
                )
                v

        validateUniqueItems : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateUniqueItems validationOptions jsonPointer v =
            when .uniqueItems
                (Decode.list Decode.value)
                (\uniqueItems list ->
                    if not uniqueItems then
                        Ok v
                    else
                        case findDuplicateItem list of
                            Just v ->
                                Err [ Error jsonPointer <| UniqueItems v ]

                            Nothing ->
                                Ok v
                )
                v

        validateContains : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateContains validationOptions jsonPointer v =
            whenSubschema .contains
                (Decode.list Decode.value)
                (\contains list ->
                    if
                        List.any
                            (\item ->
                                case validateSchema validationOptions jsonPointer item contains of
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

        validateMaxProperties : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMaxProperties validationOptions jsonPointer v =
            when .maxProperties
                (Decode.keyValuePairs Decode.value)
                (\maxProperties obj ->
                    let
                        x =
                            List.length obj
                    in
                        if x <= maxProperties then
                            Ok v
                        else
                            Err [ Error jsonPointer <| MaxProperties maxProperties x ]
                )
                v

        validateMinProperties : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMinProperties validationOptions jsonPointer v =
            when .minProperties
                (Decode.keyValuePairs Decode.value)
                (\minProperties obj ->
                    let
                        x =
                            List.length obj
                    in
                        if x >= minProperties then
                            Ok v
                        else
                            Err [ Error jsonPointer <| MinProperties minProperties x ]
                )
                v

        validateRequired : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateRequired validationOptions jsonPointer v s =
            when .required
                (Decode.keyValuePairs Decode.value)
                (\required obj ->
                    let
                        keys =
                            obj
                                |> Debug.log "validate required"
                                |> List.map (\( key, _ ) -> key)

                        missing =
                            required
                                |> List.filter (flip List.member keys >> not)
                    in
                        if List.isEmpty missing then
                            Ok v
                        else
                            Err [ Error jsonPointer <| Required missing ]
                )
                v
                s

        addDefaultProperties : ValidationOptions -> JsonPointer -> Maybe Schemata -> List ( String, Value ) -> List ( String, Value )
        addDefaultProperties validationOptions jsonPointer properties obj =
            let
                missing name obj =
                    obj
                        |> List.filter (\( n, _ ) -> n == name)
                        |> List.isEmpty

                defaultFor obj propName schema =
                    if obj |> missing propName then
                        case schema of
                            ObjectSchema os ->
                                os.default
                                    |> Maybe.andThen
                                        (\value ->
                                            validateSchema { validationOptions | applyDefaults = False } { jsonPointer | path = jsonPointer.path ++ [ propName ] } value schema
                                                |> Result.toMaybe
                                        )

                            _ ->
                                Nothing
                    else
                        Nothing
            in
                if validationOptions.applyDefaults then
                    case properties of
                        Just (Schemata knownProps) ->
                            knownProps
                                |> List.foldl
                                    (\( propName, schema ) resultingObject ->
                                        case defaultFor obj propName schema of
                                            Just value ->
                                                ( propName, value ) :: resultingObject

                                            Nothing ->
                                                resultingObject
                                    )
                                    []
                                |> List.reverse

                        _ ->
                            []
                else
                    []

        validateProperties : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateProperties validationOptions jsonPointer v subSchema =
            when .properties
                (Decode.keyValuePairs Decode.value)
                (\properties obj ->
                    let
                        revObj =
                            obj |> List.reverse

                        newProps =
                            addDefaultProperties validationOptions jsonPointer subSchema.properties revObj

                        addedPropNames =
                            newProps
                                |> List.map (\( name, _ ) -> name)
                                |> Debug.log "addedPropNames"

                        upgradedObject =
                            revObj
                                ++ newProps
                                |> Debug.log "upgradedObject"
                    in
                        upgradedObject
                            |> List.map
                                (\( key, value ) ->
                                    if List.member key addedPropNames then
                                        Ok value
                                    else
                                        case getSchema key properties of
                                            Just schema ->
                                                validateSchema validationOptions { jsonPointer | path = jsonPointer.path ++ [ key ] } value schema

                                            Nothing ->
                                                Ok value
                                )
                            |> concatErrors (upgradedObject |> Encode.object |> Ok)
                )
                v
                subSchema
                |> Debug.log ("validateProperties result at " ++ (toString jsonPointer))

        {-
           Validation succeeds if, for each instance name that matches any regular
           expressions that appear as a property name in this keyword's value, the
           child instance for that name successfully validates against each schema
           that corresponds to a matching regular expression.

           Excerpt from http://json-schema.org/latest/json-schema-validation.html#rfc.section.6.19
        -}
        validatePatternProperties : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validatePatternProperties validationOptions jsonPointer v =
            when .patternProperties
                (Decode.keyValuePairs Decode.value)
                (\(Schemata patternProperties) obj ->
                    List.foldl
                        (\( pattern, schema ) res ->
                            case res of
                                Ok _ ->
                                    obj
                                        |> getPropsByPattern pattern
                                        |> List.map
                                            (\( key, value ) ->
                                                validateSchema validationOptions { jsonPointer | path = jsonPointer.path ++ [ key ] } value schema
                                            )
                                        |> concatErrors (Ok v)

                                Err _ ->
                                    res
                        )
                        (Ok v)
                        patternProperties
                )
                v

        validateAdditionalProperties : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateAdditionalProperties validationOptions jsonPointer v s =
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
                                                        validateSchema validationOptions { jsonPointer | path = jsonPointer.path ++ [ key ] } val additionalProperties
                                                    )
                                                |> concatErrors (Ok v)
                               )
                    )
                    v
                    s

        validateDependencies : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateDependencies validationOptions jsonPointer v s =
            let
                validateDep obj =
                    s.dependencies
                        |> List.foldl
                            (\( depName, dep ) res ->
                                case res of
                                    Err _ ->
                                        res

                                    Ok _ ->
                                        if Dict.member depName (Dict.fromList obj) then
                                            case dep of
                                                PropSchema ss ->
                                                    validateSchema validationOptions jsonPointer v ss

                                                ArrayPropNames keys ->
                                                    validateSchema validationOptions jsonPointer v (ObjectSchema { blankSubSchema | required = Just keys })
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

        validatePropertyNames : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validatePropertyNames validationOptions jsonPointer v =
            let
                validatePropertyName schema key =
                    case validateSchema validationOptions { jsonPointer | path = jsonPointer.path ++ [ key ] } (Encode.string key) schema of
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

        validateEnum : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateEnum validationOptions jsonPointer =
            when .enum
                Decode.value
                (\enum val ->
                    if List.any (\item -> stringify item == (stringify val)) enum then
                        Ok val
                    else
                        Err [ Error jsonPointer Enum ]
                )

        validateConst : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateConst validationOptions jsonPointer =
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
                            Ok val
                        else
                            Err [ Error jsonPointer Const ]
                )

        validateType : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateType validationOptions jsonPointer val s =
            case s.type_ of
                AnyType ->
                    Ok val

                SingleType st ->
                    validateSingleType validationOptions jsonPointer st val

                NullableType st ->
                    case validateSingleType validationOptions jsonPointer NullType val of
                        Err _ ->
                            validateSingleType validationOptions jsonPointer st val

                        _ ->
                            Ok val

                UnionType listTypes ->
                    if List.any (\st -> validateSingleType validationOptions jsonPointer st val == (Ok val)) listTypes then
                        Ok val
                    else
                        Err [ Error jsonPointer <| InvalidType "None of desired types match" ]

        validateSingleType : ValidationOptions -> JsonPointer -> SingleType -> Value -> Result (List Error) Value
        validateSingleType validationOptions jsonPointer st val =
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

        validateAllOf : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateAllOf validationOptions jsonPointer =
            when .allOf
                Decode.value
                (\allOf val ->
                    List.foldl
                        (\schema res ->
                            if res == (Ok val) then
                                validateSchema validationOptions jsonPointer val schema
                            else
                                res
                        )
                        (Ok val)
                        allOf
                )

        validateAnyOf : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateAnyOf validationOptions jsonPointer =
            when .anyOf
                Decode.value
                (\anyOf val ->
                    let
                        validationResults =
                            anyOf
                                |> List.map (validateSchema validationOptions jsonPointer val)

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

        validateOneOf : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateOneOf validationOptions jsonPointer =
            when .oneOf
                Decode.value
                (\oneOf val ->
                    let
                        validSubschema schema =
                            validateSchema validationOptions jsonPointer val schema == (Ok val)
                    in
                        case oneOf |> List.filter validSubschema |> List.length of
                            1 ->
                                Ok val

                            0 ->
                                Err [ Error jsonPointer OneOfNoneSucceed ]

                            len ->
                                Err [ Error jsonPointer <| OneOfManySucceed len ]
                )

        validateNot : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateNot validationOptions jsonPointer =
            whenSubschema .not
                Decode.value
                (\notSchema val ->
                    if validateSchema validationOptions jsonPointer val notSchema == (Ok val) then
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
        validateSchema validationOptions (JsonPointer "" []) value schema


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
