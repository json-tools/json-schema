module Types exposing (..)

import JsonSchema
import Json.Decode exposing (Value)


type alias ClientSettings =
    { service : String
    , vault : String
    , secretKey : String
    , guide : Bool
    }

type alias RequestConfig =
    { method : String
    , baseUrl : String
    , pathname : String
    , auth : Maybe String
    , body : Maybe Value
    }

type alias PersistedData =
    { clientSettings : Maybe ClientSettings
    }


type alias Id =
    String


type alias Schema =
    JsonSchema.Schema


