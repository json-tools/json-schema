module Services.ServiceDescriptor exposing (list, listRequest, get)

import HttpBuilder exposing (Response, Error, RequestBuilder)
import Task exposing (Task)
import Json.Decode as Decode exposing (Decoder, Value, string, (:=))
import Types exposing (ClientSettings, Id)
import Models exposing (ServiceDescriptor)
import Util exposing (fetch, buildAuthHeader)
import JsonSchema as JS exposing (decodeSchema)


listRequest : Value -> ClientSettings -> RequestBuilder
listRequest body clientSettings =
    HttpBuilder.get (clientSettings.service ++ "/services")
        |> HttpBuilder.withHeaders
           [ ( "Authorization", buildAuthHeader clientSettings.secretKey )
           , ( "Accept", "application/json" )
           ]
        |> HttpBuilder.withJsonBody body


list : ClientSettings -> Task (Error String) (Response (List ServiceDescriptor))
list =
    fetch "/services" (Decode.at [ "data" ] <| Decode.list decodeService)


get : Id -> ClientSettings -> Task (Error String) (Response ServiceDescriptor)
get id =
    fetch ("/services/" ++ id) decodeService

decodeService : Decoder ServiceDescriptor
decodeService =
    Decode.object4 ServiceDescriptor
        ("id" := string)
        ("name" := string)
        ("type" := string)
        ("schema" := JS.decodeSchema)


