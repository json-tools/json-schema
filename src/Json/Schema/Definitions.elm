module Json.Schema.Definitions
    exposing
        ( Schema(IntegerSchema, StringSchema, FloatSchema, Undefined)
        , decoder
        , NumberValidations
        , StringValidations
        )

import Json.Decode as Decode exposing (Decoder, maybe, nullable, field, andThen, string, float, int, succeed)
import Json.Decode.Pipeline exposing (decode, optional)


type Schema
    = IntegerSchema NumberValidations
    | FloatSchema NumberValidations
    | StringSchema StringValidations
    | Undefined NumberValidations StringValidations


type alias NumberValidations =
    { multipleOf : Maybe Float
    , maximum : Maybe Float
    , exclusiveMaximum : Maybe Float
    , minimum : Maybe Float
    , exclusiveMinimum : Maybe Float
    }


type alias StringValidations =
    { maxLength : Maybe Int
    , minLength : Maybe Int
    , pattern : Maybe String
    }


decoder : Decoder Schema
decoder =
    string
        |> field "type"
        |> maybe
        |> andThen typedDecoder


typedDecoder : Maybe String -> Decoder Schema
typedDecoder t =
    case t of
        Just "integer" ->
            Decode.map IntegerSchema numValidationsDecoder

        Just "number" ->
            Decode.map FloatSchema numValidationsDecoder

        Just "string" ->
            Decode.map StringSchema strValidationsDecoder

        _ ->
            Decode.map2 Undefined
                numValidationsDecoder
                strValidationsDecoder


numValidationsDecoder : Decoder NumberValidations
numValidationsDecoder =
    decode NumberValidations
        |> optional "multipleOf" (nullable float) Nothing
        |> optional "maximum" (nullable float) Nothing
        |> optional "exclusiveMaximum" (nullable float) Nothing
        |> optional "minimum" (nullable float) Nothing
        |> optional "exclusiveMinimum" (nullable float) Nothing


strValidationsDecoder : Decoder StringValidations
strValidationsDecoder =
    decode StringValidations
        |> optional "maxLength" (nullable int) Nothing
        |> optional "minLength" (nullable int) Nothing
        |> optional "pattern" (nullable string) Nothing
