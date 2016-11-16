module Types exposing (..)

import JsonSchema
import Json.Decode exposing (Value)

type alias ClientSettings =
    { service : String
    , vault : String
    , secretKey : String
    , guide : Bool
    }

type alias Config =
    { endpoints : List (String, ApiEndpointDefinition)
    }

type alias ApiEndpointDefinition =
    { method : String
    , service : String
    , pathname : String
    , auth : Bool
    , request : Value
    , response : Value
    }

type alias RequestConfig =
    { method : String
    , pathname : String
    , auth : Bool
    }

type alias PersistedData =
    { clientSettings : Maybe ClientSettings
    }


type alias Id =
    String


type alias Schema =
    JsonSchema.Schema


