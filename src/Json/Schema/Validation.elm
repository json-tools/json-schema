module Json.Schema.Validation exposing
    ( validate, ValidationOptions, defaultOptions
    , Error, ValidationError(..), JsonPointer
    )

{-|


# Validate

Validation fails with list of errors, one for each invalid leaf of the value object.
When validation succeeds it also returns value being validated. Currently this value is the same as initial value, later version will allow options to be supplied in order to normalize value along the validation (e.g. apply defaults, remove additional properties, coerce types)

@docs validate, ValidationOptions, defaultOptions


# Validation Errors

@docs Error, ValidationError, JsonPointer

-}

import Dict
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode exposing (float, int, string)
import Json.Schema.Definitions
    exposing
        ( Dependency(..)
        , ExclusiveBoundary(..)
        , Items(..)
        , Schema(..)
        , Schemata(..)
        , SingleType(..)
        , SubSchema
        , Type(..)
        , blankSchema
        , blankSubSchema
        , decoder
        )
import Json.Schemata as Schemata
import Ref exposing (SchemataPool, resolveReference)
import Regex
import String.UTF32 as UTF32
import Util exposing (indexOfFirstDuplicate, isInt)


{-| Validation options which allow to apply defaults (more options upcoming)
-}
type alias ValidationOptions =
    { applyDefaults : Bool
    }


{-| Default validation options, applyDefaults = True
-}
defaultOptions : ValidationOptions
defaultOptions =
    { applyDefaults = False
    }


{-| Path in json value.

A few notes:

  - empty list represents root
  - indices of array items are integers encoded as string

-}
type alias JsonPointer =
    { ns : String
    , path : List String
    }


{-| Attempt to validate returns `Result` with list of `Error` instances as an `Err`.
-}
type alias Error =
    { jsonPointer : JsonPointer
    , details : ValidationError
    }


{-| Validation errors with details. The rule of parametrized errors like `Maximum` is that first parameter is always expected value, second parameter is actual value. Most of errors named after respective validation properties, only exception from this rule for cases like `AlwaysFail` which doesn't have keyword (this is result of boolean schema false), or `AdditionalPropertiesDisallowed` which represent subset of `.additionalProperties` validation when its value equals to `false` and additional property is present.

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
    | RequiredProperty
    | AdditionalPropertiesDisallowed (List String)
    | AdditionalPropertyDisallowed
    | InvalidPropertyName (List Error)
    | Enum
    | Const
    | InvalidType String
    | OneOfNoneSucceed (List (Result (List Error) Value))
    | OneOfManySucceed (List (Result (List Error) Value))
    | Not
    | UnresolvableReference String
    | AlwaysFail


{-| Validate value against schema
-}
validate : ValidationOptions -> SchemataPool -> Value -> Schema -> Schema -> Result (List Error) Value
validate validationOptions pool value rootSchema schema =
    let
        validateSubschema validationOptionsLocal jsonPointer os valueLocal =
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
                |> failWithListErrors validationOptionsLocal jsonPointer valueLocal os

        validateSchema validationOptionsLocal jsonPointer valueLocal s =
            case s of
                BooleanSchema bs ->
                    if bs then
                        Ok valueLocal

                    else
                        Err [ Error jsonPointer AlwaysFail ]

                ObjectSchema os ->
                    case os.ref of
                        Just ref ->
                            case ref |> resolveReference jsonPointer.ns pool rootSchema of
                                Just ( ns, ObjectSchema oss ) ->
                                    validateSubschema validationOptionsLocal { jsonPointer | ns = ns } oss valueLocal

                                Just ( ns, BooleanSchema bs ) ->
                                    if bs then
                                        Ok valueLocal

                                    else
                                        Err [ Error jsonPointer AlwaysFail ]

                                Nothing ->
                                    Err [ Error jsonPointer <| UnresolvableReference ref ]

                        Nothing ->
                            validateSubschema validationOptionsLocal jsonPointer os valueLocal

        failWithListErrors : ValidationOptions -> JsonPointer -> Value -> SubSchema -> List (ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value) -> Result (List Error) Value
        failWithListErrors validationOptionsLocal jsonPointer valueLocal schemaLocal validators =
            validators
                |> List.foldl
                    (\fn ( errors, val ) ->
                        case fn validationOptionsLocal jsonPointer val schemaLocal of
                            Ok newValue ->
                                ( errors, newValue )

                            Err list ->
                                ( errors ++ list, val )
                    )
                    ( [], valueLocal )
                |> (\( errors, v ) ->
                        case errors of
                            [] ->
                                Ok v

                            list ->
                                Err list
                   )

        validateMultipleOf : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMultipleOf validationOptionsLocal jsonPointer v =
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
        validateMaximum validationOptionsLocal jsonPointer v s =
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
        validateMinimum validationOptionsLocal jsonPointer v s =
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
        validateExclusiveMaximum validationOptionsLocal jsonPointer v s =
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
        validateExclusiveMinimum validationOptionsLocal jsonPointer v s =
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
        validateMaxLength validationOptionsLocal jsonPointer v =
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
        validateMinLength validationOptionsLocal jsonPointer v =
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
        validatePattern validationOptionsLocal jsonPointer v =
            when .pattern
                Decode.string
                (\pattern str ->
                    if Regex.contains (Regex.fromString pattern |> Maybe.withDefault Regex.never) str then
                        Ok v

                    else
                        Err [ Error jsonPointer <| Pattern pattern str ]
                )
                v

        validateItems : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateItems validationOptionsLocal jsonPointer valueLocal schemaLocal =
            let
                validateItem item schemaLocalLocal index =
                    validateSchema validationOptionsLocal { jsonPointer | path = jsonPointer.path ++ [ index |> String.fromInt ] } item schemaLocalLocal
                        |> Result.map (\_ -> index + 1)
            in
            case schemaLocal.items of
                ItemDefinition itemSchema ->
                    case Decode.decodeValue (Decode.list Decode.value) valueLocal of
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
                                |> Result.map (\_ -> valueLocal)

                        Err _ ->
                            Ok valueLocal

                ArrayOfItems listItemSchemas ->
                    case Decode.decodeValue (Decode.list Decode.value) valueLocal of
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
                                                        case schemaLocal.additionalItems of
                                                            Just itemSchema ->
                                                                validateItem item itemSchema index

                                                            Nothing ->
                                                                Ok (index + 1)

                                            _ ->
                                                res
                                    )
                                    (Ok 0)
                                |> Result.map (\_ -> valueLocal)

                        Err _ ->
                            Ok valueLocal

                _ ->
                    Ok valueLocal

        validateMaxItems : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateMaxItems validationOptionsLocal jsonPointer v =
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
        validateMinItems validationOptionsLocal jsonPointer v =
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
        validateUniqueItems validationOptionsLocal jsonPointer v =
            when .uniqueItems
                (Decode.list Decode.value)
                (\uniqueItems list ->
                    if not uniqueItems then
                        Ok v

                    else
                        case findDuplicateItem list of
                            Just vv ->
                                Err [ Error jsonPointer <| UniqueItems vv ]

                            Nothing ->
                                Ok v
                )
                v

        validateContains : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateContains validationOptionsLocal jsonPointer v =
            whenSubschema .contains
                (Decode.list Decode.value)
                (\contains list ->
                    if
                        List.any
                            (\item ->
                                case validateSchema validationOptionsLocal jsonPointer item contains of
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
        validateMaxProperties validationOptionsLocal jsonPointer v =
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
        validateMinProperties validationOptionsLocal jsonPointer v =
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
        validateRequired validationOptionsLocal jsonPointer v s =
            when .required
                (Decode.keyValuePairs Decode.value)
                (\required obj ->
                    let
                        keys =
                            obj
                                |> List.map (\( key, _ ) -> key)

                        missing =
                            required
                                |> List.filter ((\a -> List.member a keys) >> not)
                    in
                    if List.isEmpty missing then
                        Ok v

                    else
                        missing
                            |> List.map
                                (\key ->
                                    Error { jsonPointer | path = jsonPointer.path ++ [ key ] } RequiredProperty
                                )
                            |> (::) (Error jsonPointer <| Required missing)
                            |> Err
                )
                v
                s

        addDefaultProperties : ValidationOptions -> JsonPointer -> Maybe Schemata -> List ( String, Value ) -> List ( String, Value )
        addDefaultProperties validationOptionsLocal jsonPointer properties obj =
            let
                missing name objLocal =
                    objLocal
                        |> List.map Tuple.first
                        |> List.member name
                        |> not

                defaultFor objLocal propName schemaLocal =
                    if objLocal |> missing propName then
                        case schemaLocal of
                            ObjectSchema os ->
                                os.default
                                    |> Maybe.andThen
                                        (\valueLocal ->
                                            validateSchema { validationOptionsLocal | applyDefaults = False } { jsonPointer | path = jsonPointer.path ++ [ propName ] } valueLocal schemaLocal
                                                |> Result.toMaybe
                                        )
                                    |> (\x ->
                                            case x of
                                                Just _ ->
                                                    x

                                                Nothing ->
                                                    if os.properties /= Nothing then
                                                        addDefaultProperties validationOptions { jsonPointer | path = jsonPointer.path ++ [ propName ] } os.properties []
                                                            |> Encode.object
                                                            |> Just

                                                    else
                                                        Nothing
                                       )

                            _ ->
                                Nothing

                    else
                        Nothing
            in
            if validationOptionsLocal.applyDefaults then
                case properties of
                    Just (Schemata knownProps) ->
                        knownProps
                            |> List.foldl
                                (\( propName, schemaLocalLocal ) resultingObject ->
                                    case defaultFor obj propName schemaLocalLocal of
                                        Just valueLocal ->
                                            ( propName, valueLocal ) :: resultingObject

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
        validateProperties validationOptionsLocal jsonPointer v subSchema =
            when .properties
                (Decode.keyValuePairs Decode.value)
                (\properties obj ->
                    let
                        newProps =
                            addDefaultProperties validationOptionsLocal jsonPointer subSchema.properties obj

                        addedPropNames =
                            newProps
                                |> List.map (\( name, _ ) -> name)

                        upgradedObject =
                            obj
                                ++ newProps
                    in
                    upgradedObject
                        |> List.map
                            (\( key, valueLocal ) ->
                                if List.member key addedPropNames then
                                    Ok valueLocal

                                else
                                    case getSchema key properties of
                                        Just schemaLocalLocal ->
                                            validateSchema validationOptionsLocal { jsonPointer | path = jsonPointer.path ++ [ key ] } valueLocal schemaLocalLocal

                                        Nothing ->
                                            Ok valueLocal
                            )
                        |> concatErrors (upgradedObject |> Encode.object |> Ok)
                )
                v
                subSchema

        {-
           Validation succeeds if, for each instance name that matches any regular
           expressions that appear as a property name in this keyword's value, the
           child instance for that name successfully validates against each schema
           that corresponds to a matching regular expression.

           Excerpt from http://json-schema.org/latest/json-schema-validation.html#rfc.section.6.19
        -}
        validatePatternProperties : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validatePatternProperties validationOptionsLocal jsonPointer v =
            when .patternProperties
                (Decode.keyValuePairs Decode.value)
                (\(Schemata patternProperties) obj ->
                    List.foldl
                        (\( pattern, schemaLocal ) res ->
                            case res of
                                Ok _ ->
                                    obj
                                        |> getPropsByPattern pattern
                                        |> List.map
                                            (\( key, valueLocal ) ->
                                                validateSchema validationOptionsLocal { jsonPointer | path = jsonPointer.path ++ [ key ] } valueLocal schemaLocal
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
        validateAdditionalProperties validationOptionsLocal jsonPointer v s =
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
                        |> rejectMatching s.patternProperties (\a b -> Regex.contains (Regex.fromString a |> Maybe.withDefault Regex.never) b)
                        |> (\objLocal ->
                                case additionalProperties of
                                    BooleanSchema bs ->
                                        if bs then
                                            Ok v

                                        else if List.isEmpty objLocal then
                                            Ok v

                                        else
                                            objLocal
                                                |> List.map
                                                    (\( name, _ ) ->
                                                        Error { jsonPointer | path = jsonPointer.path ++ [ name ] } AdditionalPropertyDisallowed
                                                    )
                                                |> (::) (Error jsonPointer <| AdditionalPropertiesDisallowed <| List.map (\( name, _ ) -> name) objLocal)
                                                |> Err

                                    ObjectSchema _ ->
                                        objLocal
                                            |> List.map
                                                (\( key, val ) ->
                                                    validateSchema validationOptionsLocal { jsonPointer | path = jsonPointer.path ++ [ key ] } val additionalProperties
                                                )
                                            |> concatErrors (Ok v)
                           )
                )
                v
                s

        validateDependencies : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateDependencies validationOptionsLocal jsonPointer v s =
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
                                                    validateSchema validationOptionsLocal jsonPointer v ss

                                                ArrayPropNames keys ->
                                                    validateSchema validationOptionsLocal jsonPointer v (ObjectSchema { blankSubSchema | required = Just keys })

                                        else
                                            res
                            )
                            (Ok v)
            in
            if List.isEmpty s.dependencies then
                Ok v

            else
                case Decode.decodeValue (Decode.keyValuePairs Decode.value) v of
                    Ok vv ->
                        validateDep vv

                    Err _ ->
                        Ok v

        validatePropertyNames : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validatePropertyNames validationOptionsLocal jsonPointer v =
            let
                validatePropertyName schemaLocal key =
                    case validateSchema validationOptionsLocal { jsonPointer | path = jsonPointer.path ++ [ key ] } (Encode.string key) schemaLocal of
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
        validateEnum validationOptionsLocal jsonPointer =
            when .enum
                Decode.value
                (\enum val ->
                    if List.any (\item -> stringify item == stringify val) enum then
                        Ok val

                    else
                        Err [ Error jsonPointer Enum ]
                )

        validateConst : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateConst validationOptionsLocal jsonPointer =
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
        validateType validationOptionsLocal jsonPointer val s =
            case s.type_ of
                AnyType ->
                    Ok val

                SingleType st ->
                    validateSingleType validationOptionsLocal jsonPointer st val

                NullableType st ->
                    case validateSingleType validationOptionsLocal jsonPointer NullType val of
                        Err _ ->
                            validateSingleType validationOptionsLocal jsonPointer st val

                        _ ->
                            Ok val

                UnionType listTypes ->
                    if List.any (\st -> validateSingleType validationOptionsLocal jsonPointer st val == Ok val) listTypes then
                        Ok val

                    else
                        Err [ Error jsonPointer <| InvalidType "None of desired types match" ]

        validateSingleType : ValidationOptions -> JsonPointer -> SingleType -> Value -> Result (List Error) Value
        validateSingleType validationOptionsLocal jsonPointer st val =
            let
                test : Decoder a -> Result (List Error) Value
                test d =
                    Decode.decodeValue d val
                        |> Result.map (\_ -> val)
                        |> Result.mapError (\s -> [ Error jsonPointer <| InvalidType (Decode.errorToString s) ])
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
        validateAllOf validationOptionsLocal jsonPointer =
            when .allOf
                Decode.value
                (\allOf val ->
                    List.foldl
                        (\schemaLocal res ->
                            if res == Ok val then
                                validateSchema validationOptionsLocal jsonPointer val schemaLocal

                            else
                                res
                        )
                        (Ok val)
                        allOf
                )

        validateAnyOf : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateAnyOf validationOptionsLocal jsonPointer =
            when .anyOf
                Decode.value
                (\anyOf val ->
                    let
                        validationResults =
                            anyOf
                                |> List.map (validateSchema validationOptionsLocal jsonPointer val)

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
        validateOneOf validationOptionsLocal jsonPointer =
            when .oneOf
                Decode.value
                (\oneOf val ->
                    let
                        validationResults =
                            oneOf |> List.map (validateSchema validationOptionsLocal jsonPointer val)

                        successfulValidations =
                            validationResults
                                |> List.filterMap Result.toMaybe
                    in
                    case successfulValidations of
                        v :: [] ->
                            Ok v

                        [] ->
                            Err [ Error jsonPointer <| OneOfNoneSucceed validationResults ]

                        len ->
                            Err [ Error jsonPointer <| OneOfManySucceed validationResults ]
                )

        validateNot : ValidationOptions -> JsonPointer -> Value -> SubSchema -> Result (List Error) Value
        validateNot validationOptionsLocal jsonPointer =
            whenSubschema .not
                Decode.value
                (\notSchema val ->
                    if validateSchema validationOptionsLocal jsonPointer val notSchema == Ok val then
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
                |> List.filter (\( k, _ ) -> Regex.contains (Regex.fromString pattern |> Maybe.withDefault Regex.never) k)

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

        when propOf decoder fn valueLocal schemaLocal =
            case propOf schemaLocal of
                Just v ->
                    case Decode.decodeValue decoder valueLocal of
                        Ok decoded ->
                            fn v decoded

                        Err s ->
                            Ok valueLocal

                Nothing ->
                    Ok valueLocal

        whenSubschema propOf decoder fn valueLocal schemaLocal =
            case propOf schemaLocal of
                Just v ->
                    case Decode.decodeValue decoder valueLocal of
                        Ok decoded ->
                            fn v decoded
                                |> Result.map (\_ -> valueLocal)

                        Err s ->
                            Ok valueLocal

                Nothing ->
                    Ok valueLocal
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
