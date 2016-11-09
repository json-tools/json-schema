module Services.Otp exposing (create)

import HttpBuilder exposing (Response, Error)
import Task
import Json.Decode as Decode exposing (Decoder, string, (:=))
import Types exposing (ClientSettings)
import Models exposing (Otp)
import Util exposing (buildAuthHeader)


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
