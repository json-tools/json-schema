module Data.Schema exposing (Schema, Validation(IntegerSchema, FloatSchema, StringSchema, Undefined), Schemata(Schemata), Meta, decoder)

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


type alias Schema =
    { meta : Meta
    , validation : Validation
    , definitions : Maybe Schemata
    , items : Maybe Items
    }


type Schemata
    = Schemata (List ( String, Schema ))


type Items
    = ItemDefinition Schema
    | ArrayOfItems (List Schema)


type Validation
    = IntegerSchema NumberValidations
    | FloatSchema NumberValidations
    | StringSchema StringValidations
    | Undefined NumberValidations StringValidations


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
                |> list
                |> field "type"
                |> andThen multipleTypesDecoder

        validationDecoder =
            Decode.oneOf [ multipleTypes, singleType ]
    in
        Decode.map4 Schema
            metaDecoder
            validationDecoder
            (maybe <| field "definitions" schemataDecoder)
            (maybe <| field "items" itemsDecoder)


itemsDecoder : Decoder Items
itemsDecoder =
    Decode.oneOf
        [ Decode.map ItemDefinition <| lazy <| \_ -> decoder
        , Decode.map ArrayOfItems ( list <| lazy <| \_ -> decoder )
        ]


multipleTypesDecoder : List String -> Decoder Validation
multipleTypesDecoder list =
    let
        decodeNullableType _ =
            Decode.map2 Undefined
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
                Decode.map IntegerSchema NumberValidations.decoder


typedDecoder : Maybe String -> Decoder Validation
typedDecoder t =
    case t of
        Just "integer" ->
            Decode.map IntegerSchema
                NumberValidations.decoder

        Just "number" ->
            Decode.map FloatSchema
                NumberValidations.decoder

        Just "string" ->
            Decode.map StringSchema
                StringValidations.decoder

        _ ->
            Decode.map2 Undefined
                NumberValidations.decoder
                StringValidations.decoder


type alias Meta =
    { title : Maybe String
    , description : Maybe String
    , default : Maybe Value
    , examples : Maybe (List Value)
    }


metaDecoder : Decoder Meta
metaDecoder =
    decode Meta
        |> optional "title" (maybe string) Nothing
        |> optional "description" (maybe string) Nothing
        |> optional "default" (maybe value) Nothing
        |> optional "examples" (maybe <| list <| value) Nothing


schemataDecoder : Decoder Schemata
schemataDecoder =
    Decode.map Schemata (Decode.keyValuePairs (lazy (\_ -> decoder)))