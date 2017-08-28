module Json.Schema.Helpers exposing (typeToString, typeToList)

import Json.Schema.Definitions
    exposing
        ( Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        )


singleTypeToString : SingleType -> String
singleTypeToString st =
    case st of
        StringType ->
            "string"

        IntegerType ->
            "integer"

        NumberType ->
            "float"

        BooleanType ->
            "boolean"

        ObjectType ->
            "object"

        ArrayType ->
            "array"

        NullType ->
            "null"


typeToString : Type -> String
typeToString t =
    case t of
        NullableType NullType ->
            "null"

        NullableType st ->
            "nullable " ++ (singleTypeToString st)

        SingleType s ->
            singleTypeToString s

        UnionType l ->
            l
                |> List.map singleTypeToString
                |> String.join ", "

        AnyType ->
            "any"


typeToList : Type -> List String
typeToList t =
    case t of
        NullableType NullType ->
            [ "null" ]

        NullableType st ->
            [ "nullable " ++ (singleTypeToString st) ]

        SingleType s ->
            [ singleTypeToString s ]

        UnionType l ->
            l
                |> List.map singleTypeToString

        AnyType ->
            [ ]
