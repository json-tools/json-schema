module Json.Schema.Builder
    exposing
        ( SchemaBuilder(SchemaBuilder)
        , buildSchema
        , updateSchema
        , toSchema
        , validate
        , withType
        , withNullableType
        , withUnionType
        , withContains
        , withDefinitions
        , withItems
        , withItem
        , withAdditionalItems
        , withProperties
        , withPatternProperties
        , withAdditionalProperties
        , withSchemaDependency
        , withPropNamesDependency
        , withPropertyNames
        , withAllOf
        , withAnyOf
        , withOneOf
        -- simple setters
        , withMaximum
        )

import Set
import Util exposing (foldResults)
import Validation
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


type SchemaBuilder
    = SchemaBuilder { errors : List String, schema : Schema }


buildSchema =
    SchemaBuilder { errors = [], schema = blankSchema }


toSchema (SchemaBuilder sb) =
    if List.isEmpty sb.errors then
        Ok sb.schema
    else
        Err <| String.join "," sb.errors


validate val sb =
    case toSchema sb of
        Ok schema ->
            Validation.validate val schema

        Err s ->
            Err <| "Schema is invalid: " ++ s


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


updateSchema fn (SchemaBuilder sb) =
    SchemaBuilder { sb | schema = fn sb.schema }


appendError e (SchemaBuilder { errors, schema }) =
    SchemaBuilder { errors = e :: errors, schema = schema }


withNullableType t =
    case stringToType t of
        Ok NullType ->
            appendError "Nullable null is not allowed"

        Ok r ->
            updateSchema (\s -> { s | type_ = NullableType r })

        Err s ->
            appendError s


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


withContains : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withContains subSchemaBuilder =
    case subSchemaBuilder |> toSchema of
        Ok sub ->
            updateSchema (\s -> { s | contains = SubSchema sub } )
        Err s ->
            appendError s


withDefinitions defs =
    updateSchema (\s -> { s | definitions = Just (Schemata defs) } )


withItems items =
    updateSchema (\s -> { s | items = ArrayOfItems items } )


withItem item =
    updateSchema (\s -> { s | items = ItemDefinition item } )


withAdditionalItems ai =
    updateSchema (\schema -> { schema | additionalItems = SubSchema ai })


withProperties defs =
    updateSchema (\schema -> { schema | properties = Just (Schemata defs) })


withPatternProperties defs =
    updateSchema (\schema -> { schema | patternProperties = Just (Schemata defs) })


withAdditionalProperties ap =
    updateSchema (\schema -> { schema | additionalProperties = SubSchema ap })


withSchemaDependency name sd =
    updateSchema (\schema -> { schema | dependencies = ( name, PropSchema sd ) :: schema.dependencies })


withPropNamesDependency name pn =
    updateSchema (\schema -> { schema | dependencies = ( name, ArrayPropNames pn ) :: schema.dependencies })


withPropertyNames pn =
    updateSchema (\schema -> { schema | propertyNames = SubSchema pn })


withAllOf ls =
    updateSchema (\schema -> { schema | allOf = Just (List.map SubSchema ls) })


withAnyOf ls =
    updateSchema (\schema -> { schema | anyOf = Just (List.map SubSchema ls) })


withOneOf ls =
    updateSchema (\schema -> { schema | oneOf = Just (List.map SubSchema ls) })

withMaximum : Float -> SchemaBuilder -> SchemaBuilder
withMaximum x =
    updateSchema (\s -> { s | maximum = Just x })
