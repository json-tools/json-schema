module Json.Schema.Builder
    exposing
        ( SchemaBuilder(SchemaBuilder)
        , buildSchema
        , boolSchema
        , toSchema
        , validate
          -- type
        , withType
        , withNullableType
        , withUnionType
          -- meta
        , withTitle
        , withDescription
        , withDefault
        , withExamples
        , withDefinitions
          -- numeric
        , withMultipleOf
        , withMaximum
        , withMinimum
        , withExclusiveMaximum
        , withExclusiveMinimum
          -- string
        , withMaxLength
        , withMinLength
        , withPattern
        , withFormat
          -- array
        , withItems
        , withItem
        , withAdditionalItems
        , withMaxItems
        , withMinItems
        , withUniqueItems
        , withContains
          -- object
        , withMaxProperties
        , withMinProperties
        , withRequired
        , withProperties
        , withPatternProperties
        , withAdditionalProperties
        , withSchemaDependency
        , withPropNamesDependency
        , withPropertyNames
          -- generic
        , withEnum
        , withConst
        , withAllOf
        , withAnyOf
        , withOneOf
        , withNot
          -- schema
        , withRef
        , withId
          -- encode
        , encode
        )

{-| Convenience API to build a valid JSON schema

# Definition
@docs SchemaBuilder

# Schema builder creation
@docs buildSchema, boolSchema, toSchema, encode

# Building up schema

## Type

JSON Schema spec allows type to be string or array of strings. There are three
groups of types produced: single types (e.g. `"string"`), nullable types (e.g. `["string", "null"]`)
and union types (e.g. `["string", "object"]`)

@docs withType, withNullableType, withUnionType

## Meta

@docs withTitle, withDescription, withDefault, withExamples, withDefinitions

## JSON-Schema

@docs withId, withRef

## Numeric validations

The following validations are only applicable to numeric values and
will always succeed for any type other than `number` and `integer`

@docs withMultipleOf, withMaximum, withMinimum, withExclusiveMaximum, withExclusiveMinimum

## String validations

@docs withMaxLength, withMinLength, withPattern, withFormat

## Array validations

@docs withItems, withItem, withAdditionalItems, withMaxItems, withMinItems, withUniqueItems, withContains

## Object validations

@docs withMaxProperties, withMinProperties, withRequired, withProperties, withPatternProperties, withAdditionalProperties, withSchemaDependency, withPropNamesDependency, withPropertyNames

## Generic validations

@docs withEnum, withConst, withAllOf, withAnyOf, withOneOf, withNot



# Validation
@docs validate

-}

import Util exposing (foldResults)
import Json.Schema.Validation as Validation exposing (Error)
import Json.Decode exposing (Value)
import Json.Encode as Encode
import Ref
import Json.Schema.Definitions
    exposing
        ( Schema(ObjectSchema, BooleanSchema)
        , ExclusiveBoundary(BoolBoundary, NumberBoundary)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, NullType, ArrayType, ObjectType, BooleanType)
        , Schemata(Schemata)
        , SubSchema
        , Items(ItemDefinition, ArrayOfItems, NoItems)
        , Dependency(ArrayPropNames, PropSchema)
        , stringToType
        , blankSubSchema
        )


{-| Builder for JSON schema providing an API like this:

    buildSchema
        |> withTitle "My object"
        |> withProperties
            [ ( "foo"
              , buildSchema
                    |> withType "string" )
            , ( "bar"
              , buildSchema
                    |> withType "integer"
                    |> withMaximum 10
              )
            ]
-}
type SchemaBuilder
    = SchemaBuilder { errors : List String, schema : Maybe SubSchema, bool : Maybe Bool }



-- BASIC API


{-| Create schema builder with blank schema
-}
buildSchema : SchemaBuilder
buildSchema =
    SchemaBuilder { errors = [], schema = Just blankSubSchema, bool = Nothing }


{-| Create boolean schema
-}
boolSchema : Bool -> SchemaBuilder
boolSchema b =
    SchemaBuilder { errors = [], schema = Nothing, bool = Just b }


{-| Extract JSON Schema from the builder
-}
toSchema : SchemaBuilder -> Result String Schema
toSchema (SchemaBuilder sb) =
    if List.isEmpty sb.errors then
        case sb.bool of
            Just x ->
                Ok <| BooleanSchema x

            Nothing ->
                case sb.schema of
                    Just ss ->
                        Ok <| ObjectSchema { ss | source = Json.Schema.Definitions.encode (ObjectSchema ss) }

                    Nothing ->
                        Ok <| ObjectSchema blankSubSchema
    else
        Err <| String.join ", " sb.errors


{-| Validate value using schema controlled by builder.
-}
validate : Validation.ValidationOptions -> Value -> SchemaBuilder -> Result (List Error) Value
validate validationOptions val sb =
    case toSchema sb of
        Ok schema ->
            Validation.validate validationOptions Ref.defaultPool val schema schema

        Err s ->
            Ok val



--Err <| "Schema is invalid: " ++ s
-- TYPE


{-| Set the `type` property of JSON schema to a specific type, accepts strings

    buildSchema
        |> withType "boolean"
-}
withType : String -> SchemaBuilder -> SchemaBuilder
withType t sb =
    t
        |> stringToType
        |> Result.map (\x -> updateSchema (\s -> { s | type_ = SingleType x }) sb)
        |> (\r ->
                case r of
                    Ok x ->
                        x

                    Err s ->
                        appendError s sb
           )


{-| Set the `type` property of JSON schema to a nullable type.

    buildSchema
        |> withNullableType "string"
-}
withNullableType : String -> SchemaBuilder -> SchemaBuilder
withNullableType t =
    case stringToType t of
        Ok NullType ->
            appendError "Nullable null is not allowed"

        Ok r ->
            updateSchema (\s -> { s | type_ = NullableType r })

        Err s ->
            appendError s


{-| Set the `type` property of JSON schema to an union type.

    buildSchema
        |> withUnionType [ "string", "object" ]
-}
withUnionType : List String -> SchemaBuilder -> SchemaBuilder
withUnionType listTypes sb =
    listTypes
        |> List.sort
        |> List.map stringToType
        |> foldResults
        |> Result.map (\s -> updateSchema (\x -> { x | type_ = UnionType s }) sb)
        |> (\x ->
                case x of
                    Err s ->
                        appendError s sb

                    Ok x ->
                        x
           )


{-| Set the `contains` property of JSON schema to a sub-schema.

    buildSchema
        |> withContains
            (buildSchema
                |> withType "string"
            )
-}
withContains : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withContains =
    updateWithSubSchema (\sub s -> { s | contains = sub })


{-|
-}
withNot : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withNot =
    updateWithSubSchema (\sub s -> { s | not = sub })


{-|
-}
withDefinitions : List ( String, SchemaBuilder ) -> SchemaBuilder -> SchemaBuilder
withDefinitions =
    updateWithSchemata (\definitions s -> { s | definitions = definitions })


{-|
-}
withItems : List SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withItems listSchemas =
    case listSchemas |> toListOfSchemas of
        Ok items ->
            updateSchema (\s -> { s | items = ArrayOfItems items })

        Err s ->
            appendError s


{-|
-}
withItem : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withItem item =
    case item |> toSchema of
        Ok itemSchema ->
            updateSchema (\s -> { s | items = ItemDefinition itemSchema })

        Err s ->
            appendError s


{-|
-}
withAdditionalItems : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withAdditionalItems =
    updateWithSubSchema (\sub s -> { s | additionalItems = sub })


{-|
-}
withProperties : List ( String, SchemaBuilder ) -> SchemaBuilder -> SchemaBuilder
withProperties =
    updateWithSchemata (\properties s -> { s | properties = properties })


{-|
-}
withPatternProperties : List ( String, SchemaBuilder ) -> SchemaBuilder -> SchemaBuilder
withPatternProperties =
    updateWithSchemata (\patternProperties s -> { s | patternProperties = patternProperties })


{-|
-}
withAdditionalProperties : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withAdditionalProperties =
    updateWithSubSchema (\sub s -> { s | additionalProperties = sub })


{-|
-}
withSchemaDependency : String -> SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withSchemaDependency name sd =
    case sd |> toSchema of
        Ok depSchema ->
            updateSchema (\s -> { s | dependencies = s.dependencies ++ [ ( name, PropSchema depSchema ) ] })

        Err s ->
            appendError s


{-|
-}
withPropNamesDependency : String -> List String -> SchemaBuilder -> SchemaBuilder
withPropNamesDependency name pn =
    updateSchema (\schema -> { schema | dependencies = ( name, ArrayPropNames pn ) :: schema.dependencies })


{-|
-}
withPropertyNames : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withPropertyNames =
    updateWithSubSchema (\propertyNames s -> { s | propertyNames = propertyNames })


{-|
-}
withAllOf : List SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withAllOf =
    updateWithListOfSchemas (\allOf s -> { s | allOf = allOf })


{-|
-}
withAnyOf : List SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withAnyOf =
    updateWithListOfSchemas (\anyOf s -> { s | anyOf = anyOf })


{-|
-}
withOneOf : List SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withOneOf =
    updateWithListOfSchemas (\oneOf s -> { s | oneOf = oneOf })


{-|
-}
withTitle : String -> SchemaBuilder -> SchemaBuilder
withTitle x =
    updateSchema (\s -> { s | title = Just x })


{-|
-}
withDescription : String -> SchemaBuilder -> SchemaBuilder
withDescription x =
    updateSchema (\s -> { s | description = Just x })


{-|
-}
withMultipleOf : Float -> SchemaBuilder -> SchemaBuilder
withMultipleOf x =
    updateSchema (\s -> { s | multipleOf = Just x })


{-|
-}
withMaximum : Float -> SchemaBuilder -> SchemaBuilder
withMaximum x =
    updateSchema (\s -> { s | maximum = Just x })


{-|
-}
withMinimum : Float -> SchemaBuilder -> SchemaBuilder
withMinimum x =
    updateSchema (\s -> { s | minimum = Just x })


{-|
-}
withExclusiveMaximum : Float -> SchemaBuilder -> SchemaBuilder
withExclusiveMaximum x =
    updateSchema (\s -> { s | exclusiveMaximum = Just (NumberBoundary x) })


{-|
-}
withExclusiveMinimum : Float -> SchemaBuilder -> SchemaBuilder
withExclusiveMinimum x =
    updateSchema (\s -> { s | exclusiveMinimum = Just (NumberBoundary x) })


{-|
-}
withMaxLength : Int -> SchemaBuilder -> SchemaBuilder
withMaxLength x =
    updateSchema (\s -> { s | maxLength = Just x })


{-|
-}
withMinLength : Int -> SchemaBuilder -> SchemaBuilder
withMinLength x =
    updateSchema (\s -> { s | minLength = Just x })


{-|
-}
withMaxProperties : Int -> SchemaBuilder -> SchemaBuilder
withMaxProperties n =
    updateSchema (\s -> { s | maxProperties = Just n })


{-|
-}
withMinProperties : Int -> SchemaBuilder -> SchemaBuilder
withMinProperties n =
    updateSchema (\s -> { s | minProperties = Just n })


{-|
-}
withMaxItems : Int -> SchemaBuilder -> SchemaBuilder
withMaxItems n =
    updateSchema (\s -> { s | maxItems = Just n })


{-|
-}
withMinItems : Int -> SchemaBuilder -> SchemaBuilder
withMinItems n =
    updateSchema (\s -> { s | minItems = Just n })


{-|
-}
withUniqueItems : Bool -> SchemaBuilder -> SchemaBuilder
withUniqueItems b =
    updateSchema (\s -> { s | uniqueItems = Just b })


{-|
-}
withPattern : String -> SchemaBuilder -> SchemaBuilder
withPattern x =
    updateSchema (\s -> { s | pattern = Just x })


{-|
-}
withFormat : String -> SchemaBuilder -> SchemaBuilder
withFormat x =
    updateSchema (\s -> { s | format = Just x })


{-|
-}
withEnum : List Value -> SchemaBuilder -> SchemaBuilder
withEnum x =
    updateSchema (\s -> { s | enum = Just x })


{-|
-}
withRequired : List String -> SchemaBuilder -> SchemaBuilder
withRequired x =
    updateSchema (\s -> { s | required = Just x })


{-|
-}
withConst : Value -> SchemaBuilder -> SchemaBuilder
withConst v =
    updateSchema (\s -> { s | const = Just v })


{-|
-}
withRef : String -> SchemaBuilder -> SchemaBuilder
withRef x =
    updateSchema (\s -> { s | ref = Just x })


{-|
-}
withExamples : List Value -> SchemaBuilder -> SchemaBuilder
withExamples x =
    updateSchema (\s -> { s | examples = Just x })


{-|
-}
withDefault : Value -> SchemaBuilder -> SchemaBuilder
withDefault x =
    updateSchema (\s -> { s | default = Just x })


{-|
-}
withId : String -> SchemaBuilder -> SchemaBuilder
withId x =
    updateSchema (\s -> { s | id = Just x })



-- HELPERS


updateSchema : (SubSchema -> SubSchema) -> SchemaBuilder -> SchemaBuilder
updateSchema fn (SchemaBuilder sb) =
    case sb.schema of
        Just ss ->
            SchemaBuilder { sb | schema = Just <| fn ss }

        Nothing ->
            SchemaBuilder sb


appendError : String -> SchemaBuilder -> SchemaBuilder
appendError e (SchemaBuilder { errors, schema, bool }) =
    SchemaBuilder { errors = e :: errors, schema = schema, bool = bool }


type alias SchemataBuilder =
    List ( String, SchemaBuilder )


toSchemata : SchemataBuilder -> Result String (List ( String, Schema ))
toSchemata =
    List.foldl
        (\( key, builder ) ->
            Result.andThen
                (\schemas ->
                    builder
                        |> toSchema
                        |> Result.map (\schema -> schemas ++ [ ( key, schema ) ])
                )
        )
        (Ok [])


toListOfSchemas : List SchemaBuilder -> Result String (List Schema)
toListOfSchemas =
    List.foldl
        (\builder ->
            Result.andThen
                (\schemas ->
                    builder
                        |> toSchema
                        |> Result.map (\schema -> schemas ++ [ schema ])
                )
        )
        (Ok [])



-- updateWithSubSchema (\sub s -> { s | contains = sub })


updateWithSubSchema : (Maybe Schema -> (SubSchema -> SubSchema)) -> SchemaBuilder -> SchemaBuilder -> SchemaBuilder
updateWithSubSchema fn subSchemaBuilder =
    case subSchemaBuilder |> toSchema of
        Ok s ->
            fn (Just s)
                |> updateSchema

        Err err ->
            appendError err


updateWithSchemata : (Maybe Schemata -> (SubSchema -> SubSchema)) -> SchemataBuilder -> SchemaBuilder -> SchemaBuilder
updateWithSchemata fn schemataBuilder =
    case schemataBuilder |> toSchemata of
        Ok schemata ->
            updateSchema (fn (Just <| Schemata schemata))

        Err s ->
            appendError s


updateWithListOfSchemas : (Maybe (List Schema) -> (SubSchema -> SubSchema)) -> List SchemaBuilder -> SchemaBuilder -> SchemaBuilder
updateWithListOfSchemas fn schemasBuilder =
    case schemasBuilder |> toListOfSchemas of
        Ok ls ->
            updateSchema (fn (Just ls))

        Err s ->
            appendError s


{-| Encode schema into a builder code (elm)
-}
encode : Int -> Schema -> String
encode level s =
    let
        indent : String
        indent =
            "\n" ++ String.repeat level "    "

        pipe : String
        pipe =
            indent ++ "|> "

        comma : String
        comma =
            indent ++ ", "

        comma2 : String
        comma2 =
            indent ++ "  , "

        comma4 : String
        comma4 =
            indent ++ "    , "

        optionally : (a -> String) -> Maybe a -> String -> String -> String
        optionally fn val key res =
            case val of
                Just s ->
                    res ++ pipe ++ key ++ " " ++ fn s

                Nothing ->
                    res

        encodeItems : Items -> String -> String
        encodeItems items res =
            case items of
                ItemDefinition id ->
                    res ++ pipe ++ "withItem " ++ (encode (level + 1) >> addParens) id

                ArrayOfItems aoi ->
                    res ++ pipe ++ "withItem " ++ (aoi |> List.map (encode (level + 1)) |> String.join comma)

                NoItems ->
                    res

        encodeDependency : String -> Dependency -> String
        encodeDependency key dep =
            case dep of
                PropSchema ps ->
                    pipe ++ "withSchemaDependency \"" ++ key ++ "\" " ++ encode (level + 1) ps

                ArrayPropNames apn ->
                    pipe
                        ++ "withPropNamesDependency \""
                        ++ key
                        ++ "\" [ "
                        ++ (apn
                                |> List.map (\s -> "\"" ++ s ++ "\"")
                                |> String.join ", "
                           )
                        ++ " ]"

        encodeDependencies : List ( String, Dependency ) -> String -> String
        encodeDependencies deps res =
            if List.isEmpty deps then
                res
            else
                res
                    ++ pipe
                    ++ "withDependencies"
                    ++ (deps
                            |> List.map (\( key, dep ) -> encodeDependency key dep)
                            |> String.join pipe
                       )

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

        encodeType : Type -> String -> String
        encodeType t res =
            case t of
                SingleType st ->
                    res ++ pipe ++ "withType \"" ++ singleTypeToString st ++ "\""

                NullableType st ->
                    res ++ pipe ++ "withNullableType \"" ++ singleTypeToString st ++ "\""

                UnionType ut ->
                    res ++ pipe ++ "withUnionType [" ++ (ut |> List.map (singleTypeToString >> toString) |> String.join ", ") ++ "]"

                AnyType ->
                    res

        encodeListSchemas : List Schema -> String
        encodeListSchemas l =
            l
                |> List.map (encode (level + 1))
                |> String.join comma2
                |> \s -> indent ++ "  [ " ++ s ++ indent ++ "  ]"

        encodeSchemata : Schemata -> String
        encodeSchemata (Schemata l) =
            l
                |> List.map (\( s, x ) -> "( \"" ++ s ++ "\"" ++ comma4 ++ encode (level + 2) x ++ indent ++ "    )")
                |> String.join comma2
                |> \s -> indent ++ "  [ " ++ s ++ indent ++ "  ]"

        addParens s =
            "( "
                ++ s
                ++ " )"
    in
        case s of
            BooleanSchema bs ->
                if bs then
                    "boolSchema True"
                else
                    "boolSchema False"

            ObjectSchema os ->
                [ encodeType os.type_
                , optionally toString os.id "withId"
                , optionally toString os.ref "withRef"
                , optionally toString os.title "withTitle"
                , optionally toString os.description "withDescription"
                , optionally (\x -> x |> Encode.encode 0 |> toString |> \x -> "(" ++ x ++ " |> Decode.decodeString Decode.value |> Result.withDefault Encode.null)") os.default "withDefault"
                , optionally (\examples -> examples |> Encode.list |> Encode.encode 0) os.examples "withExamples"
                , optionally encodeSchemata os.definitions "withDefinitions"
                , optionally toString os.multipleOf "withMultipleOf"
                , optionally toString os.maximum "withMaximum"
                , optionally toString os.exclusiveMaximum "withExclusiveMaximum"
                , optionally toString os.minimum "withMinimum"
                , optionally toString os.exclusiveMinimum "withExclusiveMinimum"
                , optionally toString os.maxLength "withMaxLength"
                , optionally toString os.minLength "withMinLength"
                , optionally toString os.pattern "withPattern"
                , optionally toString os.format "withFormat"
                , encodeItems os.items
                , optionally (encode (level + 1) >> addParens) os.additionalItems "withAdditionalItems"
                , optionally toString os.maxItems "withMaxItems"
                , optionally toString os.minItems "withMinItems"
                , optionally (Encode.bool >> toString) os.uniqueItems "withUniqueItems"
                , optionally (encode (level + 1) >> addParens) os.contains "withContains"
                , optionally toString os.maxProperties "withMaxProperties"
                , optionally toString os.minProperties "withMinProperties"
                , optionally (\s -> s |> List.map Encode.string |> Encode.list |> Encode.encode 0) os.required "withRequired"
                , optionally encodeSchemata os.properties "withProperties"
                , optionally encodeSchemata os.patternProperties "withPatternProperties"
                , optionally (encode (level + 1) >> addParens) os.additionalProperties "withAdditionalProperties"
                , encodeDependencies os.dependencies
                , optionally (encode (level + 1) >> addParens) os.propertyNames "withPropertyNames"
                , optionally (\examples -> examples |> Encode.list |> Encode.encode 0 |> \x -> "( " ++ x ++ " |> List.map Encode.string )") os.enum "withEnum"
                , optionally (Encode.encode 0 >> addParens) os.const "withConst"
                , optionally encodeListSchemas os.allOf "withAllOf"
                , optionally encodeListSchemas os.anyOf "withAnyOf"
                , optionally encodeListSchemas os.oneOf "withOneOf"
                , optionally (encode (level + 1) >> addParens) os.not "withNot"
                ]
                    |> List.foldl identity "buildSchema"
