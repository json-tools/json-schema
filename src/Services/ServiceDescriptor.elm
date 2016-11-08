module Services.ServiceDescriptor exposing (list, get)

import HttpBuilder exposing (Response, Error)
import Task exposing (Task)
import Json.Decode as Decode exposing (Decoder, string, (:=))
import Types exposing (ServiceApiConfig, Id)
import Models exposing (ServiceDescriptor)
import Util exposing (fetch)
import JsonSchema as JS exposing (decodeSchema)


list : ServiceApiConfig -> Task (Error String) (Response (List ServiceDescriptor))
list =
    fetch "/services" (Decode.at [ "data" ] <| Decode.list decodeService)


get : Id -> ServiceApiConfig -> Task (Error String) (Response ServiceDescriptor)
get id =
    fetch ("/services/" ++ id) decodeService

decodeService : Decoder ServiceDescriptor
decodeService =
    Decode.object4 ServiceDescriptor
        ("id" := string)
        ("name" := string)
        ("type" := string)
        ("schema" := JS.decodeSchema)


