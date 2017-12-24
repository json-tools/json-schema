module Ref exposing (resolveReference, parseJsonPointer, defaultPool, SchemataPool)

import Regex exposing (regex, HowMany(All))
import Json.Decode as Decode
import Json.Schema.Definitions exposing (Schema(ObjectSchema, BooleanSchema), SubSchema, Schemata(Schemata), decoder)
import Dict exposing (Dict)
import Json.Schemata as Schemata


{-|
Pool of schemata used in refs lookup by id
-}
type alias SchemataPool =
    Dict String Schema


{-| Default schemata pool containing schemata draft-04 and draft-06
-}
defaultPool : SchemataPool
defaultPool =
    Dict.empty
        |> Dict.insert "http://json-schema.org/draft-06/schema" Schemata.draft6
        |> Dict.insert "http://json-schema.org/draft-04/schema" Schemata.draft4


parseJsonPointer : String -> String -> ( Bool, String, List String )
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
            case String.split "#" pointer |> Debug.log ("split of " ++ pointer) of
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
                            |> Debug.log "case 3.1"
                    else if isAbsolute a then
                        ( a, b )
                            |> Debug.log "case 3.2"
                    else
                        ( merge currentNamespace a, b )
                            |> Debug.log "case 3.3"

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


makeJsonPointer : ( Bool, String, List String ) -> String
makeJsonPointer ( isPointer, ns, path ) =
    if isPointer then
        ("#" :: path)
            |> String.join "/"
            |> (++) ns
    else
        path
            |> String.join "/"
            |> (++) (ns ++ "#")


resolveReference : String -> SchemataPool -> Schema -> String -> Maybe ( String, Schema )
resolveReference ns pool schema ref =
    let
        rootNs =
            schema
                |> whenObjectSchema
                |> Maybe.andThen .id
                |> Maybe.withDefault ns

        resolveRecursively namespace limit schema ref =
            let
                ( isPointer, ns, path ) =
                    parseJsonPointer (Debug.log "resolving ref" ref) namespace
                        |> Debug.log "new json pointer"

                --|> Debug.log ("parse " ++ (toString ref) ++ " within ns " ++ (toString namespace))
                newJsonPointer =
                    makeJsonPointer ( isPointer, ns, path )
            in
                if limit > 0 then
                    (if isPointer then
                        pool
                            |> Dict.get (ns ++ "#")
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
                                                                resolveRecursively ns (limit - 1) schema r

                                                            Nothing ->
                                                                Just ( ns, def )

                                                    BooleanSchema _ ->
                                                        Just ( ns, def )
                                            )
                                )
                     else
                        pool
                            |> Dict.get newJsonPointer
                            |> Maybe.map (\x -> ( ns, x ))
                    )
                else
                    Just ( ns, schema )
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
