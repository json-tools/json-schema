module Ref exposing (resolveReference)

import Regex exposing (regex, HowMany(All))
import Json.Decode as Decode
import Json.Schema.Definitions exposing (Schema(ObjectSchema, BooleanSchema), SubSchema, Schemata(Schemata), SchemataPool, decoder)
import Dict


parseJsonPointer : String -> String -> ( String, List String )
parseJsonPointer pointer currentNamespace =
    let
        merge base relative =
            if isAbsolute base && hasFragments base then
                base |> Regex.replace All lastFragment (\_ -> "/" ++ relative)
            else
                relative

        hasFragments =
            Regex.contains lastFragment

        isAbsolute =
            Regex.contains absoluteUri

        ( ns, hash ) =
            case String.split "#" pointer of
                [] ->
                    ( currentNamespace, "" )

                a :: [] ->
                    if a == "" then
                        ( currentNamespace, "" )
                    else if isAbsolute a then
                        ( a, "" )
                    else
                        ( merge currentNamespace a, "" )

                a :: b :: _ ->
                    if a == "" then
                        ( currentNamespace, b )
                    else if isAbsolute a then
                        ( a, b )
                    else
                        ( merge currentNamespace a, b )
    in
        ( ns
        , hash
            |> String.split "/"
            |> List.drop 1
            |> List.map unescapeJsonPathFragment
        )


absoluteUri : Regex.Regex
absoluteUri =
    regex "\\/\\/|^\\/"


lastFragment : Regex.Regex
lastFragment =
    regex "\\/[^\\/]*$"


tilde : Regex.Regex
tilde =
    regex "~0"


slash : Regex.Regex
slash =
    regex "~1"


percent : Regex.Regex
percent =
    regex "%25"


unescapeJsonPathFragment : String -> String
unescapeJsonPathFragment s =
    s
        |> Regex.replace All tilde (\_ -> "~")
        |> Regex.replace All slash (\_ -> "/")
        |> Regex.replace All percent (\_ -> "%")


resolveReference : String -> SchemataPool -> Schema -> String -> Maybe ( String, Schema )
resolveReference ns pool schema ref =
    let
        getSchema ns =
            pool
                |> Dict.get ns
                |> Maybe.withDefault schema

        resolveRecursively namespace limit schema ref =
            let
                ( ns, path ) =
                    parseJsonPointer ref namespace
                        |> Debug.log ("parse " ++ (toString ref) ++ " within ns " ++ (toString namespace))
            in
                if limit > 0 then
                    ns
                        |> getSchema
                        |> whenObjectSchema
                        |> Maybe.andThen
                            (\os ->
                                os.source
                                    |> Decode.decodeValue (Decode.at path decoder)
                                    |> Result.toMaybe
                                    |> Maybe.andThen
                                        (\def ->
                                            case def of
                                                ObjectSchema oss ->
                                                    case oss.ref of
                                                        Just r ->
                                                            resolveRecursively ns (limit - 1) schema r

                                                        Nothing ->
                                                            Just ( ns, def )

                                                BooleanSchema _ ->
                                                    Just ( ns, def )
                                        )
                            )
                else
                    Just ( ns, schema )
    in
        resolveRecursively (schema |> whenObjectSchema |> Maybe.andThen .id |> Maybe.withDefault ns) 10 schema ref


whenObjectSchema : Schema -> Maybe SubSchema
whenObjectSchema schema =
    case schema of
        ObjectSchema os ->
            Just os

        BooleanSchema _ ->
            Nothing
