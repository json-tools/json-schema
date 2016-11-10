module Services.Pan exposing (create, createFake)

import HttpBuilder exposing (Response, Error)
import Task
import Json.Decode as Decode exposing (Decoder, string, (:=))
import Json.Encode as Encode
import Types exposing (ClientSettings)
import Models exposing (Pan, Otp, FakePan)
import Util exposing (buildAuthHeader)


create : Otp -> String -> ClientSettings -> Task.Task (Error String) (Response Pan)
create otp pan clientSettings =
    let
        auth =
            buildAuthHeader clientSettings.secretKey

        resource =
            clientSettings.vault ++ "/pan"

        successReader =
            HttpBuilder.jsonReader decodePan

        body = Encode.object
            [ ("otp", Encode.string otp.id)
            , ("pan", Encode.string pan)
            ]

        decodePan =
            Decode.object2 Pan
                ("id" := string)
                ("key" := string)
    in
        HttpBuilder.post resource
            |> HttpBuilder.withHeader "Authorization" auth
            |> HttpBuilder.withJsonBody body
            |> HttpBuilder.send successReader HttpBuilder.stringReader

createFake : String -> ClientSettings -> Task.Task (Error String) (Response FakePan)
createFake panId clientSettings =
    let
        auth =
            buildAuthHeader clientSettings.secretKey

        resource =
            clientSettings.vault ++ "/pan/fake"

        successReader =
            HttpBuilder.jsonReader decodePan

        body = Encode.object
            [ ("pan_id", Encode.string panId)
            ]

        decodePan =
            Decode.at [ "pan" ] string
    in
        HttpBuilder.post resource
            |> HttpBuilder.withHeader "Authorization" auth
            |> HttpBuilder.withJsonBody body
            |> HttpBuilder.send successReader HttpBuilder.stringReader
