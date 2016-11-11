module Services.Pan exposing (create, createRaw, createFake)

import HttpBuilder exposing (Response, Error)
import Task
import Json.Decode as Decode exposing (Decoder, string, (:=), Value)
import Json.Encode as Encode
import Types exposing (ClientSettings)
import Models exposing (Pan, Otp, FakePan)
import Util exposing (buildAuthHeader)


createRaw : Otp -> String -> ClientSettings -> Task.Task (Error Value) (Response Value)
createRaw otp pan clientSettings =
    let
        auth =
            buildAuthHeader clientSettings.secretKey

        resource =
            clientSettings.vault ++ "/pan"

        readAsValue =
            HttpBuilder.jsonReader Decode.value

        body = Encode.object
            [ ("otp", Encode.string otp.id)
            , ("pan", Encode.string pan)
            ]

    in
        HttpBuilder.post resource
            |> HttpBuilder.withHeader "Authorization" auth
            |> HttpBuilder.withJsonBody body
            |> HttpBuilder.send readAsValue readAsValue

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
            [ ("panId", Encode.string panId)
            ]

        decodePan =
            Decode.at [ "pan" ] string
    in
        HttpBuilder.post resource
            |> HttpBuilder.withHeader "Authorization" auth
            |> HttpBuilder.withJsonBody body
            |> HttpBuilder.send successReader HttpBuilder.stringReader
