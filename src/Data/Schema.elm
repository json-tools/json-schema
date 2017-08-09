module Data.Schema exposing (Schema(IntegerSchema, FloatSchema, StringSchema, Undefined), Schemata(Schemata), Meta, decoder)

import Data.NumberValidations as NumberValidations exposing (NumberValidations)
import Data.StringValidations as StringValidations exposing (StringValidations)
import Json.Decode as Decode
    exposing
        ( Value
        , Decoder
        , succeed
        , lazy
        , maybe
        , nullable
        , field
        , andThen
        , string
        , list
        , value
        )
import Json.Decode.Pipeline exposing (decode, optional)
import Json.Decode.Extra as DecodeExtra exposing ((|:))


type Schema
    = IntegerSchema Meta NumberValidations
    | FloatSchema Meta NumberValidations
    | StringSchema Meta StringValidations
    | Undefined Meta NumberValidations StringValidations


decoder : Decoder Schema
decoder =
    let
        singleType =
            string
                |> field "type"
                |> maybe
                |> andThen typedDecoder

        multipleTypes =
            string
                |> Decode.list
                |> field "type"
                |> andThen multipleTypesDecoder
    in
        Decode.oneOf [ multipleTypes, singleType ]


multipleTypesDecoder : List String -> Decoder Schema
multipleTypesDecoder list =
    let
        decodeNullableType _ =
            Decode.map3 Undefined
                metaDecoder
                NumberValidations.decoder
                StringValidations.decoder
    in
        case list of
            [ x, "null" ] ->
                decodeNullableType x

            [ "null", x ] ->
                decodeNullableType x

            [ x ] ->
                typedDecoder <| Just x

            _ ->
                Decode.map2 IntegerSchema metaDecoder NumberValidations.decoder


typedDecoder : Maybe String -> Decoder Schema
typedDecoder t =
    case t of
        Just "integer" ->
            Decode.map2 IntegerSchema metaDecoder NumberValidations.decoder

        Just "number" ->
            Decode.map2 FloatSchema metaDecoder NumberValidations.decoder

        Just "string" ->
            Decode.map2 StringSchema metaDecoder StringValidations.decoder

        _ ->
            Decode.map3 Undefined
                metaDecoder
                NumberValidations.decoder
                StringValidations.decoder


type alias Meta =
    { title : Maybe String
    , description : Maybe String
    , default : Maybe Value
    , examples : Maybe (List Value)
    }


type Schemata
    = Schemata (List ( String, Schema ))


metaDecoder : Decoder Meta
metaDecoder =
    succeed Meta
        |: (maybe <| field "title" string)
        |: (maybe <| field "description" string)
        |: (maybe <| field "default" value)
        |: (maybe <| field "examples" (list <| value))


schemataDecoder : Decoder Schemata
schemataDecoder =
    Decode.map Schemata <| Decode.keyValuePairs <| lazy  <| \_ -> decoder
