module Json.Schema
    exposing
        ( Schema
        , ArrayItemDefinition(ArrayItemDefinition)
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
@docs Schema, ArrayItemDefinition, Properties

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
import Dict exposing (Dict)
import String


{-| Schema represents a node of schema.
Leaf types are `string`, `int`, and `boolean`.
Composite types are `array` and `object`.
Array item type defined in `items`.
Object properties defined in `properties`.
-}
type alias Schema =
    { type_ : String
    , required : Set.Set String
    , default : Maybe Value
    , description : String
    , format : Maybe String
    , ref : Maybe String
    , enum : Maybe (List String)
    , items : Maybe ArrayItemDefinition
    , properties : Properties
    , definitions : Properties
    }


{-| ArrayItemDefinition - wrapper type for recursive schema node,
defines type of array items
-}
type ArrayItemDefinition
    = ArrayItemDefinition Schema


{-| Properties - list of properties definitions for node with type = object
-}
type Properties
    = Properties (List ( String, Schema ))


{-| Empty schema object, handy as default or initial value
-}
empty : Schema
empty =
    Schema
        -- type_
        "any"
        -- required []
        Set.empty
        -- default
        Nothing
        -- description
        ""
        -- format
        Nothing
        -- ref
        Nothing
        -- enum
        Nothing
        -- items
        Nothing
        -- properties
        (Properties [])
        -- definitions
        (Properties [])


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
fromValue : Value -> Result String Schema
fromValue val =
    Decode.decodeValue decodeSchema val
        |> Result.andThen convert


decodeSchema : Decoder Schema
decodeSchema =
    succeed Schema
        |: (withDefault "" <| field "type" string)
        |: (withDefault Set.empty <| field "required" (DecodeExtra.set string))
        |: (maybe <| field "default" Decode.value)
        |: withDefault "" (field "description" string)
        |: (maybe <| field "format" string)
        |: (maybe <| field "$ref" string)
        |: (maybe <| field "enum" (Decode.list string))
        |: (maybe <| field "items" (lazy (\_ -> Decode.map ArrayItemDefinition decodeSchema)))
        |: (withDefault (Properties []) <| field "properties" (lazy (\_ -> decodeProperties)))
        |: (withDefault (Properties []) <| field "definitions" (lazy (\_ -> decodeProperties)))


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
                (Properties props) =
                    node.properties

                tryNext ( key, prop ) list =
                    case walk prop of
                        Ok p ->
                            Ok (( key, p ) :: list)

                        Err s ->
                            Err s

                newProps =
                    List.foldl
                        (\item res -> res |> Result.andThen (tryNext item))
                        (Ok [])
                        props
            in
                case newProps of
                    Ok props ->
                        Ok
                            { node | properties = Properties props }

                    Err s ->
                        Err s

        updateArrayItemDef node =
            case node.items of
                Nothing ->
                    Ok node

                Just (ArrayItemDefinition def) ->
                    case walk def of
                        Ok newDef ->
                            Ok { node | items = Just (ArrayItemDefinition newDef) }

                        Err s ->
                            Err s

        clarifyType node =
            let
                checkEnum node =
                    case node.enum of
                        Nothing ->
                            checkItems node

                        Just _ ->
                            { node | type_ = "string" }

                checkItems node =
                    case node.items of
                        Nothing ->
                            checkProperties node node.properties

                        Just _ ->
                            { node | type_ = "array" }

                checkProperties node (Properties p) =
                    if List.isEmpty p then
                        { node | type_ = "any" }
                    else
                        { node | type_ = "object" }
            in
                if String.isEmpty node.type_ || node.type_ == "any" then
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
        |> setValue simpleSchema [ "foo" ] ( Encode.string "bar" )
        |> Expect.equal ( Ok ( object [ ( "foo", Encode.string "bar" ) ] ) )
-}
setValue : Schema -> List String -> Value -> Value -> Result String Value
setValue schema subPath finalValue dataNode =
    case subPath of
        [] ->
            Ok finalValue

        key :: tail ->
            case schema.type_ of
                "object" ->
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
                                Err "Key not found"

                "array" ->
                    let
                        index =
                            String.toInt key |> Result.withDefault 0

                        nodeList =
                            decodeList dataNode

                    in
                        case schema.items of
                            Nothing ->
                                Err "No items definition"

                            Just (ArrayItemDefinition prop) ->
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
                    (Ok finalValue)


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
                |> Set.toList
                |> List.map defaultChildProp

        defaultDefault =
            case schema.type_ of
                "object" ->
                    Encode.object objectFromRequiredProps

                "array" ->
                    Encode.list []

                "integer" ->
                    Encode.int 0

                "boolean" ->
                    Encode.bool False

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


getDefinition : Properties -> String -> Maybe Schema
getDefinition (Properties defs) name =
    List.foldl
        (\( n, prop ) result ->
            if name == n then
                Just prop
            else
                result
        )
        Nothing
        defs


{-| getValue
-}
getValue : Schema -> List String -> Value -> Value
getValue schema path inputData =
    case path of
        [] ->
            inputData

        key :: tail ->
            case schema.type_ of
                "object" ->
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

                "array" ->
                    let
                        i =
                            --Debug.log "array index is"
                            String.toInt key |> Result.withDefault 0
                    in
                        case schema.items of
                            Just (ArrayItemDefinition def) ->
                                inputData
                                    |> decodeList
                                    |> List.drop i
                                    |> List.head
                                    |> Maybe.withDefault (Encode.string "")
                                    |> getValue def tail

                            Nothing ->
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
            List.foldl (\( n, _ ) res -> res || name == n) False list

        upgrade (Properties props) =
            if hasProperty name props then
                List.map
                    (\( n, p ) ->
                        if n == name then
                            ( n, prop )
                        else
                            ( n, p )
                    )
                    props
            else
                ( name, prop ) :: props
    in
        { schema | properties = Properties <| upgrade schema.properties }
