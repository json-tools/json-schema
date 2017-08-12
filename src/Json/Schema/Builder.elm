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


withType t (SchemaBuilder { errors, schema }) =
    t
        |> stringToType
        |> Result.map (\x -> { schema | type_ = SingleType x })
        |> (\r ->
            case r of
                Ok x ->
                    { errors = errors, schema = x }
                Err s ->
                    { errors = s :: errors, schema = schema }
            )
        |> SchemaBuilder


withNullableType t schema =
    t
        |> stringToType
        |> Result.map (\r ->
            case r of
                NullType ->
                    { schema | type_ = SingleType NullType }

                r ->
                    { schema | type_ = NullableType r }
        )



withUnionType listTypes schema =
    listTypes
        |> List.sort
        |> List.map stringToType
        |> foldResults
        |> Result.map (\x -> { schema | type_ = UnionType x })


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
