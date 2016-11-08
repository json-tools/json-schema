module Types exposing (..)

import JsonSchema
import Json.Decode


type alias ServiceApiConfig =
    { apiHost : String
    , clientSecretKey : String
    }


type alias PersistedData =
    { serviceApi : ServiceApiConfig
    }


type alias Id =
    String


type alias Schema =
    JsonSchema.Schema


type alias Value =
    Json.Decode.Value
