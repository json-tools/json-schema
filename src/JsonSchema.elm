module JsonSchema exposing (..)

import Set
import Json.Decode.Extra as DecodeExtra exposing ((|:), withDefault, lazy)
import Json.Decode as Decode exposing (Decoder, maybe, string, bool, succeed, (:=))
import Json.Encode as Encode exposing (Value)
import Dict
import String


type alias Schema =
    { type_ : String
    , required : Set.Set String
    , format : Maybe String
    , ref : Maybe String
    , enum : Maybe (List String)
    , items : Maybe ArrayItemDefinition
    , properties : Properties
    , definitions : Properties
    }


type ArrayItemDefinition
    = ArrayItemDefinition (Schema)


type Properties
    = Properties (List ( String, Schema ))


empty : Schema
empty =
    Schema
        -- type_
        "any"
        -- required []
        Set.empty
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


fromString : String -> Result String Schema
fromString str =
    Decode.decodeString decodeSchema str
        `Result.andThen` convert


fromValue : Value -> Result String Schema
fromValue val =
    Decode.decodeValue decodeSchema val
        `Result.andThen` convert


decodeSchema : Decoder Schema
decodeSchema =
    succeed Schema
        |: (withDefault "" ("type" := string))
        |: (withDefault Set.empty ("required" := DecodeExtra.set string))
        |: (maybe ("format" := string))
        |: (maybe ("$ref" := string))
        |: (maybe ("enum" := (Decode.list string)))
        |: (maybe ("items" := (lazy (\_ -> Decode.map ArrayItemDefinition decodeSchema))))
        |: (withDefault (Properties []) ("properties" := (lazy (\_ -> decodeProperties))))
        |: (withDefault (Properties []) ("definitions" := (lazy (\_ -> decodeProperties))))


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
                        (\item res -> res `Result.andThen` tryNext item)
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

                        Just enum ->
                            { node | type_ = "string" }

                checkItems node =
                    case node.items of
                        Nothing ->
                            checkProperties node node.properties

                        Just items ->
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
                `Result.andThen` updateProperties
                `Result.andThen` updateArrayItemDef
                `Result.andThen` clarifyType
    in
        walk rootSchema


setValue : Schema -> List String -> Value -> Value -> Value
setValue schema subPath finalValue dataNode =
    let
        result =
            case subPath of
                [] ->
                    finalValue

                key :: tail ->
                    let
                        subSchema =
                            (getDefinition schema.properties key)
                    in
                        case schema.type_ of
                            "object" ->
                                let
                                    node =
                                        (decodeDict dataNode)

                                    value =
                                        withDefaultFor schema (Dict.get key node)
                                in
                                    Dict.insert key
                                        (case subSchema of
                                            Just prop ->
                                                setValue
                                                    prop
                                                    tail
                                                    finalValue
                                                    value

                                            Nothing ->
                                                finalValue
                                        )
                                        node
                                        |> encodeDict

                            "array" ->
                                let
                                    i =
                                        Debug.log "array index is"
                                            (String.toInt key |> Result.withDefault 0)

                                    list =
                                        (decodeList dataNode)

                                    len =
                                        List.length list

                                    update list =
                                        let
                                            updated =
                                                List.indexedMap
                                                    (\index item ->
                                                        if index == i then
                                                            case schema.items of
                                                                Just (ArrayItemDefinition prop) ->
                                                                    setValue
                                                                        prop
                                                                        tail
                                                                        finalValue
                                                                        item

                                                                Nothing ->
                                                                    -- TODO: this should be Err
                                                                    finalValue
                                                        else
                                                            item
                                                    )
                                                    list
                                        in
                                            updated
                                in
                                    (if len > i then
                                        Debug.log "just upd" list
                                     else
                                        (case schema.items of
                                            Just (ArrayItemDefinition prop) ->
                                                list
                                                    ++ [ defaultFor prop ]

                                            Nothing ->
                                                list
                                        )
                                    )
                                        |> update
                                        |> Encode.list

                            _ ->
                                finalValue
    in
        result


getInt : Schema -> List String -> Value -> Int
getInt schema path value =
    getValue schema path value
        |> Decode.decodeValue Decode.int
        |> Result.withDefault 0


getString : Schema -> List String -> Value -> String
getString schema path value =
    getValue schema path value
        |> Decode.decodeValue string
        |> Result.withDefault ""


getLength : Schema -> List String -> Value -> Int
getLength schema path value =
    getValue schema path value
        |> decodeList
        |> List.length


defaultFor : Schema -> Value
defaultFor schema =
    case schema.type_ of
        "object" ->
            Encode.object []

        "array" ->
            Encode.list []

        _ ->
            Encode.string ""


encodeDict : Dict.Dict String Value -> Value
encodeDict dict =
    Encode.object (Dict.toList dict)


decodeDict : Value -> Dict.Dict String Value
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


getValue : Schema -> List String -> Value -> Value
getValue schema path inputData =
    (case path of
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
                            (String.toInt key |> Result.withDefault 0)
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
    )


withDefaultFor : Schema -> Maybe Value -> Value
withDefaultFor schema =
    Maybe.withDefault <| defaultFor schema


mapProperties : Properties -> (( String, Schema ) -> a) -> List a
mapProperties (Properties props) fn =
    List.map fn props


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
