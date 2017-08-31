module Json.Schema.Helpers exposing (typeToString, typeToList, implyType, setValue)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Value, decodeValue, decodeString)
import Json.Encode as Encode
import Validation
import Json.Schema.Definitions
    exposing
        ( Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        , Schema(ObjectSchema, BooleanSchema)
        , Items(ArrayOfItems, ItemDefinition, NoItems)
        , SubSchema
        , Schemata(Schemata)
        , blankSubSchema
        )


type alias ImpliedType =
    { type_ : Type
    , schema : SubSchema
    , error : Maybe String
    }


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
            []


whenObjectSchema : Schema -> Maybe SubSchema
whenObjectSchema schema =
    case schema of
        ObjectSchema os ->
            Just os

        BooleanSchema _ ->
            Nothing


parseJsonPointer : String -> List String
parseJsonPointer subpath =
    subpath
        |> String.split "/"
        |> List.drop 1
        |> List.filter ((/=) "")


implyType : Value -> Schema -> String -> ImpliedType
implyType val schema subpath =
    let
        path =
            parseJsonPointer subpath

        actualValue =
            val
                |> Decode.decodeValue (Decode.at path Decode.value)
                |> Result.toMaybe
    in
        path
            |> List.foldl (weNeedToGoDeeper schema) (Just schema)
            |> Maybe.andThen whenObjectSchema
            |> Maybe.andThen (calcSubSchemaType actualValue schema)
            |> (\x ->
                case x of
                    Nothing ->
                        { type_ = AnyType
                        , schema = blankSubSchema
                        , error = Just <| "Can't imply type: " ++ subpath
                        }

                    Just (t, os) ->
                        { type_ = t
                        , schema = os
                        , error = Nothing
                        }
                )


getPropertyValue : List String -> Value -> Value
getPropertyValue path value =
    value
        |> Decode.decodeValue (Decode.at path Decode.value)
        |> Result.withDefault Encode.null


setPropertyValue : String -> Value -> List String -> Schema -> Value -> Result String Value
setPropertyValue key value path schema object =
    let
        jsonPointer =
            "#/" ++ (String.join "/" path)

        nodeType =
            implyType object schema jsonPointer

        updateOrAppend list =
            if List.any (\( k, _ ) -> k == key) list then
                list
                    |> List.map
                        (\( k, v ) ->
                            if k == key then
                                ( key, value )
                            else
                                ( k, v )
                        )
            else
                list ++ [ ( key, value ) ]
    in
        case nodeType.type_ of
            SingleType ObjectType ->
                object
                    |> decodeValue (Decode.keyValuePairs Decode.value)
                    |> Result.withDefault []
                    |> List.reverse
                    |> updateOrAppend
                    |> Encode.object
                    |> Ok

            SingleType ArrayType ->
                object
                    |> decodeValue (Decode.list Decode.value)
                    |> Result.withDefault []
                    |> (\list ->
                            let
                                index =
                                    key
                                        |> decodeString Decode.int
                                        |> Result.withDefault 0
                            in
                                if List.length list < index - 1 then
                                    list
                                        |> List.indexedMap (\i v ->
                                            if i == index then
                                                value
                                            else
                                                v
                                        )
                                else
                                    list ++ [ value ]
                       )
                    |> Encode.list
                    |> Ok

            x ->
                case nodeType.error of
                    Just e ->
                        Err e

                    Nothing ->
                        Err <| "Unable to indentify type of this node, " ++ (toString x)


setValue : Value -> String -> Value -> Schema -> Result String Value
setValue hostValue jsonPath valueToSet schema =
    let
        path =
            jsonPath
                |> parseJsonPointer
                |> List.reverse
    in
        case path of
            [] ->
                Ok valueToSet

            key :: subpath ->
                path
                    |> List.foldl
                        (\key ( path, value ) ->
                            let
                                p =
                                    List.reverse path

                                v =
                                    value
                                        |> Result.andThen
                                            (\vv ->
                                                hostValue
                                                    |> getPropertyValue p
                                                    |> setPropertyValue key vv p schema
                                            )
                            in
                                case path of
                                    [] ->
                                        ( [], v )

                                    head :: tail ->
                                        ( tail, v )
                        )
                        ( subpath, Ok valueToSet )
                    |> (\( _, v ) -> v)


setValue_ : Schema -> Schema -> String -> Value -> Value -> Result String Value
setValue_ rootSchema subSchema subpath finalValue dataNode =
    let
        schemaType =
            implyType dataNode subSchema "#/"

        -- |> Debug.log ("implied type for path " ++ subpath)
    in
        case subSchema of
            BooleanSchema _ ->
                Err "Can not set value using boolean schema"

            ObjectSchema schema ->
                case parseJsonPointer subpath of
                    [] ->
                        let
                            tryDecoding decoder =
                                case Decode.decodeValue decoder finalValue of
                                    Ok _ ->
                                        Ok finalValue

                                    Err x ->
                                        Err x
                        in
                            case schemaType.type_ of
                                (SingleType IntegerType) ->
                                    tryDecoding Decode.int

                                (SingleType StringType) ->
                                    tryDecoding Decode.string

                                (SingleType BooleanType) ->
                                    tryDecoding Decode.bool

                                _ ->
                                    Ok finalValue

                    key :: tail ->
                        case schemaType.type_ of
                            (SingleType ObjectType) ->
                                let
                                    nodeDict =
                                        decodeDict dataNode

                                    value =
                                        nodeDict
                                            |> Dict.get key
                                            |> withDefaultFor (ObjectSchema schema)

                                    updatedValue prop =
                                        setValue_
                                            rootSchema
                                            prop
                                            ("#/" ++ (String.join "/" tail))
                                            finalValue
                                            value
                                in
                                    case getDefinition schema.properties key of
                                        Just prop ->
                                            case updatedValue prop of
                                                Ok val ->
                                                    nodeDict
                                                        |> Dict.insert key val
                                                        |> encodeDict
                                                        |> \v -> Ok v

                                                Err e ->
                                                    Err e

                                        Nothing ->
                                            Err ("Key '" ++ key ++ "' not found")

                            (SingleType ArrayType) ->
                                let
                                    index =
                                        String.toInt key |> Result.withDefault 0

                                    nodeList =
                                        decodeList dataNode
                                in
                                    case schema.items of
                                        NoItems ->
                                            Err "No items definition"

                                        -- TODO: handle me correctly
                                        ArrayOfItems _ ->
                                            Err "No items definition"

                                        ItemDefinition prop ->
                                            case getListItem index nodeList of
                                                Just oldItem ->
                                                    case setValue_ rootSchema prop ("#/" ++ (String.join "/" tail)) finalValue oldItem of
                                                        Ok newValue ->
                                                            setListItem index newValue nodeList
                                                                |> Encode.list
                                                                |> \v -> Ok v

                                                        Err e ->
                                                            Err e

                                                Nothing ->
                                                    nodeList
                                                        ++ [ defaultFor prop |> setValue_ rootSchema prop ("#/" ++ (String.join "/" tail)) finalValue |> Result.withDefault (defaultFor prop) ]
                                                        |> Encode.list
                                                        |> \v -> Ok v

                            x ->
                                Err <| "Something is not right: " ++ (toString x)


getListItem : Int -> List a -> Maybe a
getListItem index list =
    let
        ( _, result ) =
            List.foldl
                (\item ( i, result ) ->
                    if index == i then
                        ( i + 1, Just item )
                    else
                        ( i + 1, result )
                )
                ( 0, Nothing )
                list
    in
        result


setListItem : Int -> a -> List a -> List a
setListItem index a list =
    List.indexedMap
        (\i item ->
            if index == i then
                a
            else
                item
        )
        list


calcSubSchemaType : Maybe Value -> Schema -> SubSchema -> Maybe (Type, SubSchema)
calcSubSchemaType actualValue schema os =
    (case os.ref of
        Just ref ->
            ref
                |> resolveReference schema
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
                            |> tryAllSchemas actualValue schema
                            |> (\res ->
                                    if res == Nothing then
                                        if os.properties /= Nothing || os.additionalProperties /= Nothing then
                                            Just (SingleType ObjectType, os)
                                        else if os.enum /= Nothing then
                                            os.enum
                                                |> deriveTypeFromEnum
                                                |> (\t -> Just (t, os))
                                        else if os == blankSubSchema then
                                            Just (AnyType, os)
                                        else
                                            Nothing
                                    else
                                        res
                               )

                    UnionType ut ->
                        if ut == [ BooleanType, ObjectType ] || ut == [ ObjectType, BooleanType ] then
                            Just (SingleType ObjectType, os)
                        else
                            Just (os.type_, os)

                    x ->
                        Just (x, os)
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


resolve : Schema -> Schema -> Schema
resolve rootSchema schema =
    schema
        |> whenObjectSchema
        |> Maybe.andThen
            (\os ->
                os.ref
                    |> Maybe.andThen (resolveReference rootSchema)
            )
        |> Maybe.withDefault schema


weNeedToGoDeeper : Schema -> String -> Maybe Schema -> Maybe Schema
weNeedToGoDeeper rootSchema key schema =
    schema
        |> Maybe.andThen whenObjectSchema
        |> Maybe.andThen
            (\os ->
                case os.ref of
                    Just r ->
                        resolveReference rootSchema r

                    Nothing ->
                        schema
            )
        |> Maybe.andThen (findProperty key rootSchema)
        |> Maybe.map (resolve rootSchema)


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
                                case def.ref of
                                    Just r ->
                                        resolveReference schema r

                                    Nothing ->
                                        Just <| ObjectSchema def
                            )
                else
                    Nothing
            )


findProperty : String -> Schema -> Schema -> Maybe Schema
findProperty name rootSchema schema =
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
                                        |> List.foldl
                                            (\s r ->
                                                if r == Nothing then
                                                    s
                                                        |> resolve rootSchema
                                                        |> findProperty name rootSchema
                                                else
                                                    r
                                            )
                                            Nothing
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


tryAllSchemas : Maybe Value -> Schema -> List Schema -> Maybe (Type, SubSchema)
tryAllSchemas actualValue rootSchema listSchemas =
    listSchemas
        |> List.map (resolve rootSchema)
        |> List.foldl
            (\schema res ->
                if res == Nothing then
                    case actualValue of
                        Just av ->
                            case Validation.validate av schema of
                                Ok _ ->
                                    schema
                                        |> whenObjectSchema
                                        |> Maybe.andThen (calcSubSchemaType actualValue rootSchema)

                                Err _ ->
                                    Nothing

                        Nothing ->
                            schema
                                |> whenObjectSchema
                                |> Maybe.andThen (calcSubSchemaType actualValue rootSchema)
                else
                    res
            )
            Nothing


withDefaultFor : Schema -> Maybe Value -> Value
withDefaultFor schema =
    Maybe.withDefault <| defaultFor schema


defaultFor : Schema -> Value
defaultFor schema =
    case schema of
        BooleanSchema _ ->
            Encode.null

        -- TODO: should be Err
        ObjectSchema schema ->
            let
                defaultChildProp propName =
                    ( propName
                    , case getDefinition schema.properties propName of
                        Just s ->
                            defaultFor s

                        Nothing ->
                            Encode.null
                    )

                objectFromRequiredProps =
                    schema.required
                        |> Maybe.map (List.map defaultChildProp)

                defaultDefault =
                    case schema.type_ of
                        SingleType ObjectType ->
                            case objectFromRequiredProps of
                                Just props ->
                                    Encode.object props

                                Nothing ->
                                    Encode.null

                        SingleType ArrayType ->
                            Encode.list []

                        SingleType IntegerType ->
                            Encode.int 0

                        SingleType BooleanType ->
                            Encode.bool False

                        SingleType NumberType ->
                            Encode.float 0

                        SingleType NullType ->
                            Encode.null

                        -- TODO: no need to assume anything, fixme
                        -- "any", "string" (assume those are strings)
                        _ ->
                            Encode.string ""
            in
                case schema.default of
                    Just default ->
                        default

                    Nothing ->
                        defaultDefault


encodeDict : Dict String Value -> Value
encodeDict dict =
    Encode.object (Dict.toList dict)


decodeDict : Value -> Dict String Value
decodeDict val =
    Decode.decodeValue (Decode.dict Decode.value) val
        |> Result.withDefault Dict.empty


decodeList : Value -> List Value
decodeList val =
    Decode.decodeValue (Decode.list Decode.value) val
        |> Result.withDefault []


getDefinition : Maybe Schemata -> String -> Maybe Schema
getDefinition defs name =
    defs
        |> Maybe.andThen
            (\(Schemata x) ->
                List.foldl
                    (\( key, prop ) result ->
                        if name == key then
                            Just prop
                        else
                            result
                    )
                    Nothing
                    x
            )
