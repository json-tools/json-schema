module Types exposing (..)

import JsonSchema
import Json.Decode exposing (Value)
import Http exposing (Response, Error)

type LogEntry
    = LogRequest RequestSettings
    | LogResponse (Response String)
    | LogError Error

type alias RequestSettings =
    { definition : ApiEndpointDefinition
    , clientSettings : ClientSettings
    , data : Maybe Value
    }

type alias ClientSettings =
    { service : String
    , vault : String
    , secretKey : String
    , guide : Bool
    }


type alias Config =
    { endpoints : List ( String, ApiEndpointDefinition )
    , dependencies : List ( String, String )
    }


type alias ApiEndpointDefinition =
    { method : String
    , service : String
    , pathname : String
    , auth : Bool
    , request : Value
    , response : Value
    , description : String
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
