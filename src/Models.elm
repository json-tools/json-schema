module Models exposing (..)

import Types exposing (Id, Schema, Value)
import Dict

type alias Context =
    { root : Schema
    , data : Value
    , errors : ValidationErrors
    }

type alias ValidationErrors =
    Dict.Dict (List String) String



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
