module Data.StringValidations exposing (StringValidations, decoder)

import Json.Decode as Decode exposing (Decoder, nullable, string, int)
import Json.Decode.Pipeline exposing (decode, optional)


type alias StringValidations =
    { maxLength : Maybe Int
    , minLength : Maybe Int
    , pattern : Maybe String
    }


decoder : Decoder StringValidations
decoder =
    decode StringValidations
        |> optional "maxLength" (nullable int) Nothing
        |> optional "minLength" (nullable int) Nothing
        |> optional "pattern" (nullable string) Nothing
