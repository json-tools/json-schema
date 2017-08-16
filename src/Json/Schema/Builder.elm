module Json.Schema.Builder
    exposing
        ( SchemaBuilder(SchemaBuilder)
        , buildSchema
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
        )

{-| Convenience API to build a valid JSON schema

# Definition
@docs SchemaBuilder

# Schema builder creation
@docs buildSchema, toSchema

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
import Validation
import Json.Decode exposing (Value)
import Data.Schema
    exposing
        ( Schema
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, NullType, ArrayType, ObjectType)
        , Schemata(Schemata)
        , SubSchema(SubSchema, NoSchema)
        , Items(ItemDefinition, ArrayOfItems, NoItems)
        , Dependency(ArrayPropNames, PropSchema)
        , stringToType
        , blankSchema
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
    = SchemaBuilder { errors : List String, schema : Schema }


-- BASIC API


{-| Create schema builder with blank schema
-}
buildSchema : SchemaBuilder
buildSchema =
    SchemaBuilder { errors = [], schema = blankSchema }


{-| Extract JSON Schema from the builder
-}
toSchema : SchemaBuilder -> Result String Schema
toSchema (SchemaBuilder sb) =
    if List.isEmpty sb.errors then
        Ok sb.schema
    else
        Err <| String.join ", " sb.errors


{-| Validate value using schema controlled by builder.
-}
validate : Value -> SchemaBuilder -> Result String Bool
validate val sb =
    case toSchema sb of
        Ok schema ->
            Validation.validate val schema

        Err s ->
            Err <| "Schema is invalid: " ++ s


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
    updateSchema (\s -> { s | title = Just x })


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
    updateSchema (\s -> { s | exclusiveMaximum = Just x })


{-|
-}
withExclusiveMinimum : Float -> SchemaBuilder -> SchemaBuilder
withExclusiveMinimum x =
    updateSchema (\s -> { s | exclusiveMinimum = Just x })


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

updateSchema : (Schema -> Schema) -> SchemaBuilder -> SchemaBuilder
updateSchema fn (SchemaBuilder sb) =
    SchemaBuilder { sb | schema = fn sb.schema }


appendError : String -> SchemaBuilder -> SchemaBuilder
appendError e (SchemaBuilder { errors, schema }) =
    SchemaBuilder { errors = e :: errors, schema = schema }


type alias SchemataBuilder =
    List ( String, SchemaBuilder )


toSchemata : SchemataBuilder -> Result String (List ( String, Schema ))
toSchemata =
    List.foldl
        (\( key, builder ) res ->
            res
                |> Result.andThen
                    (\l ->
                        builder
                            |> toSchema
                            |> Result.map (\s -> l ++ [ ( key, s ) ])
                    )
        )
        (Ok [])


toListOfSchemas : List SchemaBuilder -> Result String (List Schema)
toListOfSchemas =
    List.foldl
        (\builder res ->
            res
                |> Result.andThen
                    (\l ->
                        builder
                            |> toSchema
                            |> Result.map (\s -> l ++ [ s ])
                    )
        )
        (Ok [])

updateWithSubSchema : (SubSchema -> (Schema -> Schema)) -> SchemaBuilder -> SchemaBuilder -> SchemaBuilder
updateWithSubSchema fn subSchemaBuilder =
    case subSchemaBuilder |> toSchema of
        Ok sub ->
            updateSchema (fn (SubSchema sub))

        Err s ->
            appendError s


updateWithSchemata : (Maybe Schemata -> (Schema -> Schema)) -> SchemataBuilder -> SchemaBuilder -> SchemaBuilder
updateWithSchemata fn schemataBuilder =
    case schemataBuilder |> toSchemata of
        Ok schemata ->
            updateSchema (fn (Just <| Schemata schemata))

        Err s ->
            appendError s


updateWithListOfSchemas : (Maybe (List SubSchema) -> (Schema -> Schema)) -> List SchemaBuilder -> SchemaBuilder -> SchemaBuilder
updateWithListOfSchemas fn schemasBuilder =
    case schemasBuilder |> toListOfSchemas of
        Ok ls ->
            updateSchema (fn (Just <| List.map SubSchema ls))

        Err s ->
            appendError s


