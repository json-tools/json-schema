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
        , withPattern
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


toSchema : SchemaBuilder -> Result String Schema
toSchema (SchemaBuilder sb) =
    if List.isEmpty sb.errors then
        Ok sb.schema
    else
        Err <| String.join "," sb.errors


toSchemata : List (String, SchemaBuilder) -> Result String (List (String, Schema))
toSchemata =
    List.foldl (\(key, builder) res ->
        res
            |> Result.andThen (\l ->
                builder
                    |> toSchema
                    |> Result.map (\s -> l ++ [ (key, s) ])
            )
    ) (Ok [])


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

updateWithSubSchema fn subSchemaBuilder =
    case subSchemaBuilder |> toSchema of
        Ok sub ->
            updateSchema (fn (SubSchema sub))

        Err s ->
            appendError s

updateWithSchemata fn schemataBuilder =
    case schemataBuilder |> toSchemata of
        Ok schemata ->
            updateSchema (fn (Just <| Schemata schemata))

        Err s ->
            appendError s


withContains : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withContains =
    updateWithSubSchema (\sub s -> { s | contains = sub } )


withDefinitions : List (String, SchemaBuilder) -> SchemaBuilder -> SchemaBuilder
withDefinitions =
    updateWithSchemata (\schemata s -> { s | definitions = schemata } )


withItems items =
    updateSchema (\s -> { s | items = ArrayOfItems items } )


withItem item =
    updateSchema (\s -> { s | items = ItemDefinition item } )


withAdditionalItems : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withAdditionalItems =
    updateWithSubSchema (\sub s -> { s | additionalItems = sub })


withProperties defs =
    updateSchema (\schema -> { schema | properties = Just (Schemata defs) })


withPatternProperties defs =
    updateSchema (\schema -> { schema | patternProperties = Just (Schemata defs) })


withAdditionalProperties : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withAdditionalProperties =
    updateWithSubSchema (\sub s -> { s | additionalProperties = sub })


withSchemaDependency name sd =
    updateSchema (\schema -> { schema | dependencies = ( name, PropSchema sd ) :: schema.dependencies })


withPropNamesDependency name pn =
    updateSchema (\schema -> { schema | dependencies = ( name, ArrayPropNames pn ) :: schema.dependencies })


withPropertyNames : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withPropertyNames =
    updateWithSubSchema (\sub s -> { s | propertyNames = sub })


withAllOf ls =
    updateSchema (\schema -> { schema | allOf = Just (List.map SubSchema ls) })


withAnyOf ls =
    updateSchema (\schema -> { schema | anyOf = Just (List.map SubSchema ls) })


withOneOf ls =
    updateSchema (\schema -> { schema | oneOf = Just (List.map SubSchema ls) })


withMaximum : Float -> SchemaBuilder -> SchemaBuilder
withMaximum x =
    updateSchema (\s -> { s | maximum = Just x })


withPattern : String -> SchemaBuilder -> SchemaBuilder
withPattern x =
    updateSchema (\s -> { s | pattern = Just x })
