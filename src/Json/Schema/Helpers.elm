module Json.Schema.Helpers exposing (typeToString, typeToList, implyType)

import Json.Decode as Decode exposing (Value)
import Validation
import Json.Schema.Definitions
    exposing
        ( Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        , Schema(ObjectSchema, BooleanSchema)
        , SubSchema
        , Schemata(Schemata)
        , blankSubSchema
        )

singleTypeToString : SingleType -> String
singleTypeToString st =
    case st of
        StringType ->
            "string"

        IntegerType ->
            "integer"

        NumberType ->
            "number"

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


whenObjectSchema : Schema -> Maybe SubSchema
whenObjectSchema schema =
    case schema of
        ObjectSchema os ->
            Just os

        BooleanSchema _ ->
            Nothing


implyType : Value -> Schema -> String -> Result String Type
implyType val schema subpath =
    let
        path : List String
        path =
            subpath
                |> String.split "/"
                |> List.drop 1

        actualValue : Maybe Value
        actualValue =
            val
                |> Decode.decodeValue (Decode.at path Decode.value)
                |> Result.toMaybe

        findProperty : String -> Schema -> Maybe Schema
        findProperty name schema =
            let
                os =
                    whenObjectSchema schema
            in
                os
                    |> Maybe.andThen .properties
                    |> Maybe.andThen
                        (\(Schemata pp) ->
                            pp
                                |> List.foldl
                                    (\( key, s ) res ->
                                        if res /= Nothing || key /= name then
                                            res
                                        else
                                            Just s
                                    )
                                    Nothing
                        )
                    |> (\r ->
                            if r == Nothing then
                                os
                                    |> Maybe.andThen .additionalProperties
                            else
                                r
                       )
                    |> (\r ->
                            if r == Nothing then
                                os
                                    |> Maybe.andThen .anyOf
                                    |> Maybe.andThen
                                        (\anyOf ->
                                            anyOf
                                                |> List.foldl (\s r ->
                                                    if r == Nothing then
                                                        s
                                                            |> resolve
                                                            |> findProperty name
                                                    else
                                                        r
                                                ) Nothing
                                        )
                            else
                                r
                       )

        findDefinition : String -> Schemata -> Maybe SubSchema
        findDefinition ref (Schemata defs) =
            defs
                |> List.foldl
                    (\( key, def ) res ->
                        if res == Nothing && ("#/definitions/" ++ key) == ref then
                            whenObjectSchema def
                        else
                            res
                    )
                    Nothing

        resolve : Schema -> Schema
        resolve schema =
            schema
                |> whenObjectSchema
                |> Maybe.andThen
                    (\os ->
                        os.ref
                            |> Maybe.andThen resolveReference
                    )
                |> Maybe.withDefault schema

        resolveReference : String -> Maybe Schema
        resolveReference ref =
            schema
                |> whenObjectSchema
                |> Maybe.andThen
                    (\os ->
                        if ref == "#" then
                            Just schema
                        else if ref |> String.startsWith "#/definitions/" then
                            os.definitions
                                |> Maybe.andThen (findDefinition ref)
                                |> Maybe.andThen
                                    (\def ->
                                        case def.ref of
                                            Just r ->
                                                resolveReference r

                                            Nothing ->
                                                Just <| ObjectSchema def
                                    )
                        else
                            Nothing
                    )


        tryAllSchemas : List Schema -> Maybe Type
        tryAllSchemas listSchemas =
            listSchemas
                |> List.map resolve
                |> List.foldl
                    (\schema res ->
                        if res == Nothing then
                            case actualValue of
                                Just av ->
                                    case Validation.validate av schema of
                                        Ok _ ->
                                            schema
                                                |> whenObjectSchema
                                                |> Maybe.andThen calcSubSchemaType

                                        Err _ ->
                                            Nothing

                                Nothing ->
                                    Nothing
                        else
                            res
                    )
                    Nothing

        calcSubSchemaType : SubSchema -> Maybe Type
        calcSubSchemaType os =
            (case os.ref of
                Just ref ->
                    ref
                        |> resolveReference
                        |> Maybe.andThen whenObjectSchema

                Nothing ->
                    Just os
            )
                |> Maybe.andThen
                    (\os ->
                        case os.type_ of
                            AnyType ->
                                [ os.anyOf
                                , os.allOf
                                , os.oneOf
                                ]
                                    |> List.map (Maybe.withDefault [])
                                    |> List.concat
                                    |> tryAllSchemas
                                    |> (\res ->
                                        if res == Nothing then
                                            if os.properties /= Nothing || os.additionalProperties /= Nothing then
                                                Just <| SingleType ObjectType
                                            else if os.enum /= Nothing then
                                                os.enum
                                                    |> deriveTypeFromEnum
                                                    |> Just
                                            else if os == blankSubSchema then
                                                Just AnyType
                                            else
                                                Nothing
                                        else
                                            res
                                        )

                            UnionType ut ->
                                if ut == [ BooleanType, ObjectType ] || ut == [ ObjectType, BooleanType ] then
                                    Just <| SingleType ObjectType
                                else
                                    Just os.type_

                            x ->
                                Just x
                    )

        deriveTypeFromValue : Value -> Maybe Type
        deriveTypeFromValue val =
            case Decode.decodeValue Decode.string val of
                Ok _ ->
                    Just <| SingleType StringType

                Err _ ->
                    Nothing

        deriveTypeFromEnum : Maybe (List Value) -> Type
        deriveTypeFromEnum enum =
            enum
                |> Maybe.andThen List.head
                |> Maybe.andThen deriveTypeFromValue
                |> Maybe.withDefault AnyType

        weNeedToGoDeeper : String -> Maybe Schema -> Maybe Schema
        weNeedToGoDeeper key schema =
            schema
                |> Maybe.andThen whenObjectSchema
                |> Maybe.andThen
                    (\os ->
                        case os.ref of
                            Just r ->
                                resolveReference r

                            Nothing ->
                                schema
                    )
                |> Maybe.andThen (findProperty key)
                |> Maybe.map resolve
    in
        path
            |> List.foldl weNeedToGoDeeper (Just schema)
            |> Maybe.andThen whenObjectSchema
            |> Maybe.andThen calcSubSchemaType
            |> Result.fromMaybe ("Can't imply type: " ++ subpath)

