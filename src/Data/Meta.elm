module Data.Meta exposing (Meta, decoder)

import Data.Schemata as Schemata exposing (Schemata)
import Json.Decode exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (optional, decode)

type alias Meta =
    { definitions : Maybe SchemasDictionary
    , title : Maybe String
    , description : Maybe String
    , default : Maybe Value
    , examples : Maybe (List Value)
    }

decoder : Decoder Meta
decoder =
    decode Meta
        |> optional "definitions" 
