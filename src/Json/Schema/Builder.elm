module Json.Schema.Builder
    exposing
        ( buildSchema
        , toSchema
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
        )

import Set
import Util exposing (foldResults)
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


withType t sb =
    t
        |> stringToType
        |> Result.map (\x -> updateSchema sb (\s -> { s | type_ = SingleType x }))
        |> (\r ->
            case r of
                Ok x ->
                    x

                Err s ->
                    appendError sb s
            )


setSchema (SchemaBuilder sb) s =
    SchemaBuilder { sb | schema = s }


updateSchema (SchemaBuilder sb) fn =
    SchemaBuilder { sb | schema = fn sb.schema }


appendError (SchemaBuilder { errors, schema }) e =
    SchemaBuilder { errors = e :: errors, schema = schema }


withNullableType t sb =
    case stringToType t of
        Ok NullType ->
            appendError sb "Nullable null is not allowed"

        Ok r ->
            updateSchema sb (\s -> { s | type_ = NullableType r })

        Err s ->
            appendError sb s


withUnionType listTypes sb =
    listTypes
        |> List.sort
        |> List.map stringToType
        |> foldResults
        |> Result.map (\s -> updateSchema sb (\x -> { x | type_ = UnionType s }))
        |> (\x ->
            case x of
                Err s ->
                    appendError sb s

                Ok x ->
                    x
           )


withContains s schema =
    Ok { schema | contains = SubSchema s }


withDefinitions defs schema =
    Ok { schema | definitions = Just (Schemata defs) }


withItems items schema =
    Ok { schema | items = ArrayOfItems items }


withItem item schema =
    Ok { schema | items = ItemDefinition item }


withAdditionalItems ai schema =
    Ok { schema | additionalItems = SubSchema ai }


withProperties defs schema =
    Ok { schema | properties = Just (Schemata defs) }


withPatternProperties defs schema =
    Ok { schema | patternProperties = Just (Schemata defs) }


withAdditionalProperties ap schema =
    Ok { schema | additionalProperties = SubSchema ap }


withSchemaDependency name sd schema =
    Ok { schema | dependencies = ( name, PropSchema sd ) :: schema.dependencies }


withPropNamesDependency name pn schema =
    Ok { schema | dependencies = ( name, ArrayPropNames pn ) :: schema.dependencies }


withPropertyNames pn schema =
    Ok { schema | propertyNames = SubSchema pn }


withAllOf ls schema =
    Ok { schema | allOf = Just (List.map SubSchema ls) }


withAnyOf ls schema =
    Ok { schema | anyOf = Just (List.map SubSchema ls) }


withOneOf ls schema =
    Ok { schema | oneOf = Just (List.map SubSchema ls) }
