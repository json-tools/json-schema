module Services.Otp exposing (create, createRequest, decodeOtp)

import HttpBuilder exposing (Response, Error, RequestBuilder)
import Task
import Json.Decode as Decode exposing (Decoder, value, string, (:=), Value)
import Json.Encode as Encode
import Types exposing (ClientSettings)
import Models exposing (Otp)
import Util exposing (buildAuthHeader)

createRequest : Value -> ClientSettings -> RequestBuilder
createRequest body clientSettings =
    HttpBuilder.post (clientSettings.vault ++ "/otp")
        |> HttpBuilder.withHeaders
           [ ( "Authorization", buildAuthHeader clientSettings.secretKey )
           , ( "Accept", "application/json" )
           ]
        |> HttpBuilder.withJsonBody body


create : ClientSettings -> Task.Task (Error String) (Response Otp)
create clientSettings =
    createRequest Encode.null clientSettings
        |> HttpBuilder.send (HttpBuilder.jsonReader decodeOtp)
            HttpBuilder.stringReader


decodeOtp : Decoder Otp
decodeOtp =
    Decode.object1 Otp
        ("id" := string)
