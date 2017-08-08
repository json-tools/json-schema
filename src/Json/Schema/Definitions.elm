module Json.Schema.Definitions
    exposing
        ( Schema(IntegerSchema, FloatSchema, Undefined)
        , decoder
        , NumSchema
        )

import Json.Decode as Decode exposing (Decoder, maybe, nullable, field, andThen, string, float, int, succeed)
import Json.Decode.Pipeline exposing (decode, optional)


type Schema
    = IntegerSchema NumSchema
    | FloatSchema NumSchema
    | Undefined


type alias NumSchema =
    { multipleOf : Maybe Float
    , maximum : Maybe Float
    , exclusiveMaximum : Maybe Float
    , minimum : Maybe Float
    , exclusiveMinimum : Maybe Float
    }


decoder : Decoder Schema
decoder =
    field "type" string
        |> andThen typedDecoder


typedDecoder : String -> Decoder Schema
typedDecoder t =
    case t of
        "integer" ->
            numSchemaDecoder
                |> andThen (\r -> succeed <| IntegerSchema r)
        "number" ->
            numSchemaDecoder
                |> andThen (\r -> succeed <| FloatSchema r)

        _ ->
            succeed Undefined


numSchemaDecoder : Decoder NumSchema
numSchemaDecoder =
    decode NumSchema
        |> optional "multipleOf" (nullable float) Nothing
        |> optional "maximum" (nullable float) Nothing
        |> optional "exclusiveMaximum" (nullable float) Nothing
        |> optional "minimum" (nullable float) Nothing
        |> optional "exclusiveMinimum" (nullable float) Nothing
