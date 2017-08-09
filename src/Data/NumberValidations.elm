module Data.NumberValidations exposing (NumberValidations, decoder)

import Json.Decode as Decode exposing (Decoder, maybe, nullable, field, andThen, string, float, int, succeed)
import Json.Decode.Pipeline exposing (decode, optional)


type alias NumberValidations =
    { multipleOf : Maybe Float
    , maximum : Maybe Float
    , exclusiveMaximum : Maybe Float
    , minimum : Maybe Float
    , exclusiveMinimum : Maybe Float
    }


decoder : Decoder NumberValidations
decoder =
    decode NumberValidations
        |> optional "multipleOf" (nullable float) Nothing
        |> optional "maximum" (nullable float) Nothing
        |> optional "exclusiveMaximum" (nullable float) Nothing
        |> optional "minimum" (nullable float) Nothing
        |> optional "exclusiveMinimum" (nullable float) Nothing
