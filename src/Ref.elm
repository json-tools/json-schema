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
                        --|> debugSubSchema "find def?"
                        |>
                            Maybe.andThen
                                (\def ->
                                    case def.ref of
                                        Just r ->
                                            resolveReference schema r

                                        Nothing ->
                                            Just <| ObjectSchema def
                                )
                else
                    Nothing
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


whenObjectSchema : Schema -> Maybe SubSchema
whenObjectSchema schema =
    case schema of
        ObjectSchema os ->
            Just os

        BooleanSchema _ ->
            Nothing
