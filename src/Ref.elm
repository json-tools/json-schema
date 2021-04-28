module Ref exposing (SchemataPool, defaultPool, parseJsonPointer, resolveReference)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Schema.Definitions exposing (Schema(..), Schemata(..), SubSchema, decoder)
import Json.Schemata as Schemata
import Regex exposing (fromString)


{-| Pool of schemata used in refs lookup by id
-}
type alias SchemataPool =
    Dict String Schema


{-| Default schemata pool containing schemata draft-04 and draft-06
-}
defaultPool : SchemataPool
defaultPool =
    Dict.empty
        |> Dict.insert "http://json-schema.org/draft-06/schema" Schemata.draft6
        |> Dict.insert "http://json-schema.org/draft-06/schema#" Schemata.draft6
        |> Dict.insert "http://json-schema.org/draft-04/schema" Schemata.draft4


parseJsonPointer : String -> String -> ( Bool, String, List String )
parseJsonPointer pointer currentNamespace =
    let
        merge base relative =
            if isAbsolute base && hasFragments base then
                base |> Regex.replace lastFragment (\_ -> "/" ++ relative)

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
                        --|> Debug.log "case 3.1"

                    else if isAbsolute a then
                        ( a, b )
                        --|> Debug.log "case 3.2"

                    else
                        ( merge currentNamespace a, b )

        --|> Debug.log "case 3.4"
        isPointer =
            hasFragments hash
    in
    ( isPointer
    , ns
    , if isPointer then
        hash
            |> String.split "/"
            |> List.drop 1
            |> List.map unescapeJsonPathFragment

      else if hash /= "" then
        [ hash ]

      else
        []
    )


absoluteUri : Regex.Regex
absoluteUri =
    fromString "\\/\\/|^\\/" |> Maybe.withDefault Regex.never


lastFragment : Regex.Regex
lastFragment =
    fromString "\\/[^\\/]*$" |> Maybe.withDefault Regex.never


tilde : Regex.Regex
tilde =
    fromString "~0" |> Maybe.withDefault Regex.never


slash : Regex.Regex
slash =
    fromString "~1" |> Maybe.withDefault Regex.never


percent : Regex.Regex
percent =
    fromString "%25" |> Maybe.withDefault Regex.never


unescapeJsonPathFragment : String -> String
unescapeJsonPathFragment s =
    s
        |> Regex.replace tilde (\_ -> "~")
        |> Regex.replace slash (\_ -> "/")
        |> Regex.replace percent (\_ -> "%")


makeJsonPointer : ( Bool, String, List String ) -> String
makeJsonPointer ( isPointer, ns, path ) =
    if isPointer then
        ("#" :: path)
            |> String.join "/"
            |> (++) ns

    else if List.isEmpty path then
        ns
        -- |> Debug.log "path was empty"

    else
        path
            |> String.join "/"
            |> (++) (ns ++ "#")


removeTrailingSlash : String -> String
removeTrailingSlash s =
    if String.endsWith "#" s then
        String.dropRight 1 s

    else
        s


resolveReference : String -> SchemataPool -> Schema -> String -> Maybe ( String, Schema )
resolveReference ns pool schema ref =
    let
        rootNs =
            schema
                |> whenObjectSchema
                |> Maybe.andThen .id
                |> Maybe.map removeTrailingSlash
                |> Maybe.withDefault ns

        resolveRecursively namespace limit localSchema localRef =
            let
                ( isPointer, localNs, path ) =
                    parseJsonPointer localRef namespace

                newJsonPointer =
                    makeJsonPointer ( isPointer, localNs, path )
            in
            if limit > 0 then
                if isPointer then
                    (if localNs == "" then
                        Just localSchema

                     else
                        pool
                            |> Dict.get localNs
                    )
                        |> Maybe.andThen whenObjectSchema
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
                                                            resolveRecursively localNs (limit - 1) localSchema r

                                                        Nothing ->
                                                            Just ( localNs, def )

                                                BooleanSchema _ ->
                                                    Just ( localNs, def )
                                        )
                            )

                else if newJsonPointer == "" then
                    Just ( "", localSchema )

                else
                    pool
                        |> Dict.get newJsonPointer
                        |> Maybe.map (\x -> ( localNs, x ))

            else
                Just ( localNs, localSchema )
    in
    resolveRecursively rootNs 10 schema ref



--|> Debug.log ("resolution result for " ++ ref ++ " " ++ rootNs)


whenObjectSchema : Schema -> Maybe SubSchema
whenObjectSchema schema =
    case schema of
        ObjectSchema os ->
            Just os

        BooleanSchema _ ->
            Nothing
