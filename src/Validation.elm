module Validation exposing (validate)

import Json.Decode as Decode exposing (Value, Decoder)
import Json.Encode as Encode exposing (int, float, string)
import Dict
import Regex
import List.Extra
import Util exposing (isInt)
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


{-| Validate value against schema
-}
validate : Value -> Schema -> Result String Bool
validate value schema =
    case schema of
        BooleanSchema bs ->
            if bs then
                Ok True
            else
                Err "Always fail"

        ObjectSchema os ->
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
                |> failWithFirstError value os


failWithFirstError : Value -> SubSchema -> List (Value -> SubSchema -> Result String Bool) -> Result String Bool
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


validateMultipleOf : Value -> SubSchema -> Result String Bool
validateMultipleOf =
    when .multipleOf
        Decode.float
        (\multipleOf x ->
            if isInt (x / multipleOf) then
                Ok True
            else
                Err <| "Value is not the multiple of " ++ (toString multipleOf)
        )


validateMaximum : Value -> SubSchema -> Result String Bool
validateMaximum =
    when .maximum
        Decode.float
        (\max x ->
            if x <= max then
                Ok True
            else
                Err <| "Value is above the maximum of " ++ (toString max)
        )


validateMinimum : Value -> SubSchema -> Result String Bool
validateMinimum =
    when .minimum
        Decode.float
        (\min x ->
            if x >= min then
                Ok True
            else
                Err <| "Value is below the minimum of " ++ (toString min)
        )


validateExclusiveMaximum : Value -> SubSchema -> Result String Bool
validateExclusiveMaximum =
    when .exclusiveMaximum
        Decode.float
        (\max x ->
            if x < max then
                Ok True
            else
                Err <| "Value is not below the exclusive maximum of " ++ (toString max)
        )


validateExclusiveMinimum : Value -> SubSchema -> Result String Bool
validateExclusiveMinimum =
    when .exclusiveMinimum
        Decode.float
        (\min x ->
            if x > min then
                Ok True
            else
                Err <| "Value is not above the exclusive minimum of " ++ (toString min)
        )


validateMaxLength : Value -> SubSchema -> Result String Bool
validateMaxLength =
    when .maxLength
        Decode.string
        (\maxLength str ->
            if String.length str <= maxLength then
                Ok True
            else
                Err <| "String is longer than expected " ++ (toString maxLength)
        )


validateMinLength : Value -> SubSchema -> Result String Bool
validateMinLength =
    when .minLength
        Decode.string
        (\minLength str ->
            if String.length str >= minLength then
                Ok True
            else
                Err <| "String is shorter than expected " ++ (toString minLength)
        )


validatePattern : Value -> SubSchema -> Result String Bool
validatePattern =
    when .pattern
        Decode.string
        (\pattern str ->
            if Regex.contains (Regex.regex pattern) str then
                Ok True
            else
                Err <| "String does not match the regex pattern"
        )


validateItems : Value -> SubSchema -> Result String Bool
validateItems value schema =
    let
        validateItem item schema index =
            validate item schema
                |> Result.mapError (\err -> "Item at index " ++ (toString index) ++ ": " ++ err)
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
                            |> Result.map (\_ -> True)

                    Err _ ->
                        Ok True

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
                            |> Result.map (\_ -> True)

                    Err _ ->
                        Ok True

            _ ->
                Ok True


validateMaxItems : Value -> SubSchema -> Result String Bool
validateMaxItems =
    when .maxItems
        (Decode.list Decode.value)
        (\maxItems list ->
            if List.length list <= maxItems then
                Ok True
            else
                Err <| "Array has more items than expected (maxItems=" ++ (toString maxItems) ++ ")"
        )


validateMinItems : Value -> SubSchema -> Result String Bool
validateMinItems =
    when .minItems
        (Decode.list Decode.value)
        (\minItems list ->
            if List.length list >= minItems then
                Ok True
            else
                Err <| "Array has less items than expected (minItems=" ++ (toString minItems) ++ ")"
        )


validateUniqueItems : Value -> SubSchema -> Result String Bool
validateUniqueItems =
    when .uniqueItems
        (Decode.list Decode.value)
        (\uniqueItems list ->
            if not uniqueItems || isUniqueItems list then
                Ok True
            else
                Err <| "Array has not unique items"
        )


validateContains : Value -> SubSchema -> Result String Bool
validateContains =
    whenSubschema .contains
        (Decode.list Decode.value)
        (\contains list ->
            if List.any (\item -> validate item contains == (Ok True)) list then
                Ok True
            else
                Err <| "Array does not contain expected value"
        )


validateMaxProperties : Value -> SubSchema -> Result String Bool
validateMaxProperties =
    when .maxProperties
        (Decode.keyValuePairs Decode.value)
        (\maxProperties obj ->
            if List.length obj <= maxProperties then
                Ok True
            else
                Err <| "Object has more properties than expected (maxProperties=" ++ (toString maxProperties) ++ ")"
        )


validateMinProperties : Value -> SubSchema -> Result String Bool
validateMinProperties =
    when .minProperties
        (Decode.keyValuePairs Decode.value)
        (\minProperties obj ->
            if List.length obj >= minProperties then
                Ok True
            else
                Err <| "Object has less properties than expected (minProperties=" ++ (toString minProperties) ++ ")"
        )


validateRequired : Value -> SubSchema -> Result String Bool
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


validateProperties : Value -> SubSchema -> Result String Bool
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


validatePatternProperties : Value -> SubSchema -> Result String Bool
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


validateAdditionalProperties : Value -> SubSchema -> Result String Bool
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


validateDependencies : Value -> SubSchema -> Result String Bool
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
                                    validate v (ObjectSchema { blankSubSchema | required = Just keys })
                        else
                            res
                    )
                    (Ok True)
    in
        if List.isEmpty s.dependencies then
            Ok True
        else
            case Decode.decodeValue (Decode.keyValuePairs Decode.value) v of
                Ok v ->
                    validateDep v

                Err _ ->
                    Ok True


validatePropertyNames : Value -> SubSchema -> Result String Bool
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


validateEnum : Value -> SubSchema -> Result String Bool
validateEnum =
    when .enum
        Decode.value
        (\enum val ->
            if List.any (\item -> toString item == (toString val)) enum then
                Ok True
            else
                Err "Value is not present in enum"
        )


validateConst : Value -> SubSchema -> Result String Bool
validateConst =
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
                    Err <| "Value doesn't equal const: expected \"" ++ expected ++ "\" but the actual value is \"" ++ actual ++ "\""
        )


validateType : Value -> SubSchema -> Result String Bool
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


validateAllOf : Value -> SubSchema -> Result String Bool
validateAllOf =
    when .allOf
        Decode.value
        (\allOf val ->
            List.foldl
                (\schema res ->
                    if res == (Ok True) then
                        validate val schema

                    else
                        res
                )
                (Ok True)
                allOf
        )


validateAnyOf : Value -> SubSchema -> Result String Bool
validateAnyOf =
    when .anyOf
        Decode.value
        (\anyOf val ->
            let
                validSubschema schema =
                    validate val schema == (Ok True)

                isValid =
                    List.any validSubschema anyOf
            in
                if isValid then
                    Ok True
                else
                    Err "None of the schemas in anyOf accept this value"
        )


validateOneOf : Value -> SubSchema -> Result String Bool
validateOneOf =
    when .oneOf
        Decode.value
        (\oneOf val ->
            let
                validSubschema schema =
                    validate val schema == (Ok True)

            in
                case oneOf |> List.filter validSubschema |> List.length of
                    1 ->
                        Ok True
                    0 ->
                        Err "None of the schemas in anyOf allow this value"
                    len ->
                        Err <| "oneOf expects value to succeed validation against exactly one schema but " ++ (toString len) ++ " validations succeeded"
        )


validateNot : Value -> SubSchema -> Result String Bool
validateNot =
    whenSubschema .not
        Decode.value
        (\notSchema val ->
            if validate val notSchema  == (Ok True) then
                Err "Successful validation for the negative schema ('not' keyword)"
            else
                Ok True
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


when propOf decoder fn value schema =
    case propOf schema of
        Just v ->
            case Decode.decodeValue decoder value of
                Ok decoded ->
                    fn v decoded

                Err s ->
                    Ok True

        Nothing ->
            Ok True


whenSubschema propOf decoder fn value schema =
    case propOf schema of
        Just v ->
            case Decode.decodeValue decoder value of
                Ok decoded ->
                    fn v decoded

                Err s ->
                    Ok True

        Nothing ->
            Ok True
