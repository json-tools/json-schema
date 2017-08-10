module Json.Schema.Builder
    exposing
        ( blankSchema
        , withType
        , withNullableType
        , withUnionType
        )

import Set
import Data.Schema
    exposing
        ( Schema
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, NullType, ArrayType, ObjectType)
        , stringToType
        )


blankSchema =
    { type_ = Nothing
    }


withType t schema =
    let
        st =
            case stringToType t of
                Ok r ->
                    SingleType r

                _ ->
                    AnyType


    in
        { schema | type_ = st }

withNullableType t schema =
    let
        nt =
            case stringToType t of
                Ok NullType ->
                    SingleType NullType

                Ok r ->
                    NullableType r

                _ ->
                    AnyType


    in
        { schema | type_ = nt }


withUnionType listTypes schema =
    let
        ut =
            listTypes
                |> List.map (stringToType >> (Result.withDefault NullType))
    in
        { schema | type_ = UnionType ut }
