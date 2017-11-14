module Ref exposing (resolveReference)

import Json.Schema.Definitions exposing (Schema(ObjectSchema, BooleanSchema), SubSchema, Schemata(Schemata))


resolveReference : Schema -> String -> Maybe Schema
resolveReference schema ref =
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
                                case def of
                                    ObjectSchema oss ->
                                        case oss.ref of
                                            Just r ->
                                                resolveReference def r

                                            Nothing ->
                                                Just def

                                    BooleanSchema _ ->
                                        Just def
                            )
                else
                    Nothing
            )


findDefinition : String -> Schemata -> Maybe Schema
findDefinition ref (Schemata defs) =
    defs
        |> List.foldl
            (\( key, def ) res ->
                if res == Nothing && ("#/definitions/" ++ key) == ref then
                    Just def
                else
                    res
            )
            Nothing


whenObjectSchema : Schema -> Maybe SubSchema
whenObjectSchema schema =
    case schema of
        ObjectSchema os ->
            Just os

        BooleanSchema _ ->
            Nothing
