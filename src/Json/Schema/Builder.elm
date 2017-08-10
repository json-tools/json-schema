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
        )


blankSchema =
    { type_ = Nothing
    }


withType t schema =
    let
        st =
            case t of
                "integer" ->
                    SingleType IntegerType

                "number" ->
                    SingleType NumberType

                "string" ->
                    SingleType StringType

                "object" ->
                    SingleType ObjectType

                "array" ->
                    SingleType ArrayType

                "null" ->
                    SingleType NullType

                _ ->
                    AnyType


    in
        { schema | type_ = st }

withNullableType t schema =
    let
        nt =
            case t of
                "integer" ->
                    NullableType IntegerType

                "number" ->
                    NullableType NumberType

                "string" ->
                    NullableType StringType

                "object" ->
                    NullableType ObjectType

                "array" ->
                    NullableType ArrayType

                "null" ->
                    SingleType NullType

                _ ->
                    AnyType


    in
        { schema | type_ = nt }


withUnionType listTypes schema =
    let
        ut =
            listTypes
                |> List.map (\t ->
                    case t of
                        "integer" ->
                            IntegerType

                        "number" ->
                            NumberType

                        "string" ->
                            StringType

                        "object" ->
                            ObjectType

                        "array" ->
                            ArrayType

                        "null" ->
                            NullType

                        _ ->
                            NullType
                    )
    in
        { schema | type_ = UnionType ut }
