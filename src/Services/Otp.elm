module Services.Otp exposing (create, createRaw, decodeOtp)

import HttpBuilder exposing (Response, Error)
import Task
import Json.Decode as Decode exposing (Decoder, value, string, (:=), Value)
import Types exposing (ClientSettings)
import Models exposing (Otp)
import Util exposing (buildAuthHeader)

createRaw : ClientSettings -> Task.Task (Error Value) (Response Value)
createRaw clientSettings =
    let
        auth =
            buildAuthHeader clientSettings.secretKey

        resource =
            clientSettings.vault ++ "/otp"

        asValue =
            HttpBuilder.jsonReader value
    in
        HttpBuilder.post resource
            |> HttpBuilder.withHeader "Authorization" auth
            |> HttpBuilder.send asValue asValue

create : ClientSettings -> Task.Task (Error String) (Response Otp)
create clientSettings =
    let
        auth =
            buildAuthHeader clientSettings.secretKey

        resource =
            clientSettings.vault ++ "/otp"

        successReader =
            HttpBuilder.jsonReader decodeOtp
    in
        HttpBuilder.post resource
            |> HttpBuilder.withHeader "Authorization" auth
            |> HttpBuilder.send successReader HttpBuilder.stringReader


decodeOtp : Decoder Otp
decodeOtp =
    Decode.object1 Otp
        ("id" := string)
