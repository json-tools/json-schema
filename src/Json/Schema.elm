module Json.Schema
    exposing
        ( Schema
        , Properties
        , empty
        , fromValue
        , fromString
        , registerProperty
        , setValue
        , getValue
        , defaultFor
        , getString
        , getInt
        , getBool
        , getLength
        , mapProperties
        )

{-| This library provides bunch of utility methods to work with JSON values using
schemas defined in json-schema format

# Definition
@docs Schema, Properties

# Schema creation
@docs empty, fromString, fromValue

# Schema manipulation
@docs registerProperty

# Reading value
@docs defaultFor, getBool, getInt, getLength, getString, getValue

# Writing value

@docs setValue

# Helpers
@docs mapProperties

-}

import Set
import Json.Decode.Extra as DecodeExtra exposing ((|:), withDefault)
import Json.Decode as Decode exposing (Decoder, maybe, string, bool, succeed, field, lazy)
import Json.Encode as Encode exposing (Value)
import Data.Schema
    exposing
        ( Items(ItemDefinition, ArrayOfItems, NoItems)
        , SubSchema(SubSchema, NoSchema)
        , Schemata(Schemata)
        , Dependency(ArrayPropNames, PropSchema)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        , blankSchema
        )
import Dict exposing (Dict)
import String
import Regex
import List.Extra


{-| Schema represents a node of schema.
Leaf types are `string`, `int`, and `boolean`.
Composite types are `array` and `object`.
Array item type defined in `items`.
Object properties defined in `properties`.
-}
type alias Schema =
    Data.Schema.Schema


{-| Properties - list of properties definitions for node with type = object
-}
type Properties
    = Properties (List ( String, Schema ))


{-| Empty schema object, handy as default or initial value
-}
empty : Schema
empty =
    Data.Schema.blankSchema


{-| Build schema from JSON string.

    "{\"properties\": {\"foo\": {\"type\": \"string\"}}"
        |> fromString
-}
fromString : String -> Result String Schema
fromString str =
    Decode.decodeString decodeSchema str
        |> Result.andThen convert


{-| Build schema from JSON value.

    Encode.object
        [ ( "properties", Encode.object
            [ ( "foo", Encode.object
                [ ( "type", Encode.string "string")
                ]
            ) ]
        ) ]
            |> fromValue
-}
fromValue : Value -> Result String Data.Schema.Schema
fromValue val =
    Decode.decodeValue Data.Schema.decoder val



--|> Result.andThen convert


decodeSchema : Decoder Schema
decodeSchema =
    Data.Schema.decoder



--    succeed Schema
--        |: (withDefault "" <| field "type" string)
--        |: (withDefault Set.empty <| field "required" (DecodeExtra.set string))
--        |: (maybe <| field "default" Decode.value)
--        |: withDefault "" (field "description" string)
--        |: (maybe <| field "format" string)
--        |: (maybe <| field "$ref" string)
--        |: (maybe <| field "enum" (Decode.list string))
--        |: (maybe <| field "items" (lazy (\_ -> Decode.map ArrayItemDefinition decodeSchema)))
--        |: (withDefault (Properties []) <| field "properties" (lazy (\_ -> decodeProperties)))
--        |: (withDefault (Properties []) <| field "definitions" (lazy (\_ -> decodeProperties)))


decodeProperties : Decoder Properties
decodeProperties =
    succeed Properties
        |: Decode.keyValuePairs (lazy (\_ -> decodeSchema))


traverse : Schema -> List String -> Maybe Schema
traverse schema path =
    case path of
        section :: name :: [] ->
            case section of
                "definitions" ->
                    getDefinition schema.definitions name

                _ ->
                    Nothing

        _ ->
            Nothing


convert : Schema -> Result String Schema
convert rootSchema =
    let
        digDefinition : String -> Schema -> Result String Schema
        digDefinition ref node =
            String.split "/" ref
                |> List.drop 1
                |> traverse rootSchema
                |> Result.fromMaybe ("Unable to find definition for " ++ ref)

        updateProperties node =
            let
                tryNext ( key, prop ) list =
                    case walk prop of
                        Ok p ->
                            Ok (( key, p ) :: list)

                        Err s ->
                            Err s

                newProps props =
                    List.foldl
                        (\item res -> res |> Result.andThen (tryNext item))
                        (Ok [])
                        props
            in
                node.properties
                    |> Maybe.map
                        (\(Schemata props) ->
                            case newProps props of
                                Ok p ->
                                    Ok
                                        { node | properties = Just (Schemata p) }

                                Err s ->
                                    Err s
                        )
                    |> Maybe.withDefault (Ok node)

        updateArrayItemDef node =
            case node.items of
                NoItems ->
                    Ok node

                -- TODO: handle me correctly
                ArrayOfItems _ ->
                    Ok node

                ItemDefinition def ->
                    case walk def of
                        Ok newDef ->
                            Ok { node | items = ItemDefinition newDef }

                        Err s ->
                            Err s

        clarifyType node =
            let
                checkEnum node =
                    case node.enum of
                        Nothing ->
                            checkItems node

                        Just _ ->
                            { node | type_ = SingleType StringType }

                checkItems node =
                    case node.items of
                        NoItems ->
                            checkProperties node node.properties

                        _ ->
                            { node | type_ = SingleType ArrayType }

                checkProperties node p =
                    if p == Nothing then
                        { node | type_ = AnyType }
                    else
                        { node | type_ = SingleType ObjectType }
            in
                if node.type_ == AnyType then
                    Ok (checkEnum node)
                else
                    Ok node

        walk : Schema -> Result String Schema
        walk node =
            (case node.ref of
                Just ref ->
                    digDefinition ref node

                Nothing ->
                    Ok node
            )
                |> Result.andThen updateProperties
                |> Result.andThen updateArrayItemDef
                |> Result.andThen clarifyType
    in
        walk rootSchema


{-| Set value of node

    Encode.object []
        |> setValue
            simpleSchema
            [ "foo" ] -- path in schema
            ( Encode.string "bar" ) -- value to set
        |> Expect.equal
            ( Ok ( object [ ( "foo", Encode.string "bar" ) ] ) )
-}
setValue : Schema -> List String -> Value -> Value -> Result String Value
setValue schema subPath finalValue dataNode =
    case subPath of
        [] ->
            let
                tryDecoding decoder =
                    case Decode.decodeValue decoder finalValue of
                        Ok _ ->
                            Ok finalValue

                        Err x ->
                            Err x
            in
                case schema.type_ of
                    SingleType IntegerType ->
                        tryDecoding Decode.int

                    SingleType StringType ->
                        tryDecoding Decode.string

                    SingleType BooleanType ->
                        tryDecoding Decode.bool

                    _ ->
                        Ok finalValue

        key :: tail ->
            case schema.type_ of
                SingleType ObjectType ->
                    let
                        nodeDict =
                            decodeDict dataNode

                        value =
                            nodeDict
                                |> Dict.get key
                                |> withDefaultFor schema

                        updatedValue prop =
                            setValue
                                prop
                                tail
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

                SingleType ArrayType ->
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
                                        case setValue prop tail finalValue oldItem of
                                            Ok newValue ->
                                                setListItem index newValue nodeList
                                                    |> Encode.list
                                                    |> \v -> Ok v

                                            Err e ->
                                                Err e

                                    Nothing ->
                                        nodeList
                                            ++ [ defaultFor prop |> setValue prop tail finalValue |> Result.withDefault (defaultFor prop) ]
                                            |> Encode.list
                                            |> \v -> Ok v

                _ ->
                    Ok finalValue


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


{-| Return int leaf
-}
getInt : Schema -> List String -> Value -> Int
getInt schema path value =
    getValue schema path value
        |> Decode.decodeValue Decode.int
        |> Result.withDefault 0


{-| Return boolean leaf
-}
getBool : Schema -> List String -> Value -> Bool
getBool schema path value =
    getValue schema path value
        |> Decode.decodeValue Decode.bool
        |> Result.withDefault False


{-| Return string leaf
-}
getString : Schema -> List String -> Value -> String
getString schema path value =
    getValue schema path value
        |> Decode.decodeValue string
        |> Result.withDefault ""


{-| Return length of array node
-}
getLength : Schema -> List String -> Value -> Int
getLength schema path value =
    getValue schema path value
        |> decodeList
        |> List.length


{-| Return default (initial) value for schema node. When default value for node
is not specified, then default calculated based on type:

  - "boolean" -> False
  - "integer" -> 0
  - "array" -> []
  - "object" -> {props listed in required}
-}
defaultFor : Schema -> Value
defaultFor schema =
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


{-| getValue
-}
getValue : Schema -> List String -> Value -> Value
getValue schema path inputData =
    case path of
        [] ->
            inputData

        key :: tail ->
            case schema.type_ of
                SingleType ObjectType ->
                    -- TODO: lookup definition using "ref" (need root schema for that)
                    case getDefinition schema.properties key of
                        Just def ->
                            inputData
                                |> decodeDict
                                |> Dict.get key
                                |> Maybe.withDefault (defaultFor def)
                                |> getValue def tail

                        Nothing ->
                            defaultFor schema

                SingleType ArrayType ->
                    let
                        i =
                            --Debug.log "array index is"
                            String.toInt key |> Result.withDefault 0
                    in
                        case schema.items of
                            ItemDefinition def ->
                                inputData
                                    |> decodeList
                                    |> List.drop i
                                    |> List.head
                                    |> Maybe.withDefault (Encode.string "")
                                    |> getValue def tail

                            NoItems ->
                                defaultFor schema

                            -- TODO: handle me correctly
                            ArrayOfItems _ ->
                                defaultFor schema

                _ ->
                    inputData


withDefaultFor : Schema -> Maybe Value -> Value
withDefaultFor schema =
    Maybe.withDefault <| defaultFor schema


{-| Iterate through properties
-}
mapProperties : Properties -> (( String, Schema ) -> a) -> List a
mapProperties (Properties props) fn =
    List.map fn props


{-| Register property
-}
registerProperty : String -> Schema -> Schema -> Schema
registerProperty name prop schema =
    let
        hasProperty name list =
            List.foldl (\( key, _ ) res -> res || name == key) False list

        upgrade : Maybe Schemata -> Maybe Schemata
        upgrade props =
            props
                |> Maybe.map
                    (\(Schemata schemata) ->
                        if hasProperty name schemata then
                            List.map
                                (\( n, p ) ->
                                    if n == name then
                                        ( n, prop )
                                    else
                                        ( n, p )
                                )
                                schemata
                        else
                            ( name, prop ) :: schemata
                    )
                |> Maybe.map Schemata
    in
        { schema | properties = upgrade schema.properties }
