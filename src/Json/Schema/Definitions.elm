module Json.Schema.Definitions
    exposing
        ( Schema(IntegerSchema, Undefined)
        , decoder
        , IntSchema
        )

import Json.Decode as Decode exposing (Decoder, maybe, nullable, field, andThen, string, float, int, succeed)
import Json.Decode.Pipeline exposing (decode, optional)


type Schema
    = IntegerSchema IntSchema
    | Undefined


type alias IntSchema =
    { maximum : Maybe Float
    , minimum : Maybe Float
    }


decoder : Decoder Schema
decoder =
    field "type" string
        |> andThen typedDecoder


typedDecoder : String -> Decoder Schema
typedDecoder t =
    case t of
        "integer" ->
            intSchemaDecoder
                |> andThen (\r -> succeed <| IntegerSchema r)

        _ ->
            succeed Undefined


intSchemaDecoder : Decoder IntSchema
intSchemaDecoder =
    decode IntSchema
        |> optional "maximum" (nullable float) Nothing
        |> optional "minimum" (nullable float) Nothing
