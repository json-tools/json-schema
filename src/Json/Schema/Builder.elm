module Json.Schema.Builder
    exposing
        ( withType
        , withNullableType
        , withUnionType
        , withContains
        )

import Set
import Util exposing (foldResults)
import Data.Schema
    exposing
        ( Schema
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, NullType, ArrayType, ObjectType)
        , SubSchema(SubSchema, NoSchema)
        , stringToType
        , blankSchema
        )


withType t schema =
    t
        |> stringToType
        |> Result.map (\x -> { schema | type_ = SingleType x })


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

