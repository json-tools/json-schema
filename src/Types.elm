module Types exposing (..)

import JsonSchema
import Json.Decode


type alias ClientSettings =
    { service : String
    , vault : String
    , secretKey : String
    , guide : Bool
    }


type alias PersistedData =
    { clientSettings : Maybe ClientSettings
    }


type alias Id =
    String


type alias Schema =
    JsonSchema.Schema


type alias Value =
    Json.Decode.Value
