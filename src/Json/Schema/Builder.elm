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
        , withTitle
        , withMultipleOf
        , withMaximum
        , withMinimum
        , withExclusiveMaximum
        , withExclusiveMinimum
        , withPattern
        , withEnum
        , withRequired
        )

import Set
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


toListOfSchemas : List SchemaBuilder -> Result String (List Schema)
toListOfSchemas =
    List.foldl (\builder res ->
        res
            |> Result.andThen (\l ->
                builder
                    |> toSchema
                    |> Result.map (\s -> l ++ [ s ])
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


updateWithListOfSchemas fn schemasBuilder =
    case schemasBuilder |> toListOfSchemas of
        Ok ls ->
            updateSchema (fn (Just <| List.map SubSchema ls))

        Err s ->
            appendError s


withContains : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withContains =
    updateWithSubSchema (\sub s -> { s | contains = sub } )


withDefinitions : List (String, SchemaBuilder) -> SchemaBuilder -> SchemaBuilder
withDefinitions =
    updateWithSchemata (\definitions s -> { s | definitions = definitions } )


withItems : List SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withItems listSchemas =
    case listSchemas |> toListOfSchemas of
        Ok items ->
            updateSchema (\s -> { s | items = ArrayOfItems items } )

        Err s ->
            appendError s


withItem : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withItem item =
    case item |> toSchema of
        Ok itemSchema ->
            updateSchema (\s -> { s | items = ItemDefinition itemSchema } )

        Err s ->
            appendError s


withAdditionalItems : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withAdditionalItems =
    updateWithSubSchema (\sub s -> { s | additionalItems = sub })


withProperties : List (String, SchemaBuilder) -> SchemaBuilder -> SchemaBuilder
withProperties =
    updateWithSchemata (\properties s -> { s | properties = properties })


withPatternProperties : List (String, SchemaBuilder) -> SchemaBuilder -> SchemaBuilder
withPatternProperties =
    updateWithSchemata (\patternProperties s -> { s | patternProperties = patternProperties })


withAdditionalProperties : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withAdditionalProperties =
    updateWithSubSchema (\sub s -> { s | additionalProperties = sub })


withSchemaDependency : String -> SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withSchemaDependency name sd =
    case sd |> toSchema of
        Ok depSchema ->
            updateSchema (\s -> { s | dependencies = s.dependencies ++ [ ( name, PropSchema depSchema ) ] })

        Err s ->
            appendError s


withPropNamesDependency : String -> List String -> SchemaBuilder -> SchemaBuilder
withPropNamesDependency name pn =
    updateSchema (\schema -> { schema | dependencies = ( name, ArrayPropNames pn ) :: schema.dependencies })


withPropertyNames : SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withPropertyNames =
    updateWithSubSchema (\propertyNames s -> { s | propertyNames = propertyNames })


withAllOf : List SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withAllOf =
    updateWithListOfSchemas (\allOf s -> { s | allOf = allOf })


withAnyOf : List SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withAnyOf =
    updateWithListOfSchemas (\anyOf s -> { s | anyOf = anyOf })


withOneOf : List SchemaBuilder -> SchemaBuilder -> SchemaBuilder
withOneOf =
    updateWithListOfSchemas (\oneOf s -> { s | oneOf = oneOf })


withTitle : String -> SchemaBuilder -> SchemaBuilder
withTitle x =
    updateSchema (\s -> { s | title = Just x })


withMultipleOf : Float -> SchemaBuilder -> SchemaBuilder
withMultipleOf x =
    updateSchema (\s -> { s | multipleOf = Just x })


withMaximum : Float -> SchemaBuilder -> SchemaBuilder
withMaximum x =
    updateSchema (\s -> { s | maximum = Just x })


withMinimum : Float -> SchemaBuilder -> SchemaBuilder
withMinimum x =
    updateSchema (\s -> { s | minimum = Just x })


withExclusiveMaximum : Float -> SchemaBuilder -> SchemaBuilder
withExclusiveMaximum x =
    updateSchema (\s -> { s | exclusiveMaximum = Just x })


withExclusiveMinimum : Float -> SchemaBuilder -> SchemaBuilder
withExclusiveMinimum x =
    updateSchema (\s -> { s | exclusiveMinimum = Just x })


withPattern : String -> SchemaBuilder -> SchemaBuilder
withPattern x =
    updateSchema (\s -> { s | pattern = Just x })


withEnum : List Value -> SchemaBuilder -> SchemaBuilder
withEnum x =
    updateSchema (\s -> { s | enum = Just x })


withRequired : List String -> SchemaBuilder -> SchemaBuilder
withRequired x =
    updateSchema (\s -> { s | required = Just x })
