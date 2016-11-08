module Models exposing (..)

import Types exposing (Id, Schema, Value)


type alias ServiceDescriptor =
    { id : Id
    , name : String
    , type' : String
    , schema : Schema
    }


type alias Job =
    { id : Id
    , state : String
    , input : Value
    , output :
        Value
        -- , errors : Value
    }
