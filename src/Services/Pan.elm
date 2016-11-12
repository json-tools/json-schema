module Services.Pan exposing (create, createRequest, createFake, createFakeRequest)

import HttpBuilder exposing (Response, Error, RequestBuilder, withHeaders)
import Task
import Json.Decode as Decode exposing (Decoder, string, (:=), Value)
import Json.Encode as Encode
import Types exposing (ClientSettings)
import Models exposing (Pan, Otp, FakePan)
import Util exposing (buildAuthHeader)


createRequest : Value -> ClientSettings -> RequestBuilder
createRequest body clientSettings =
    let
        auth =
            buildAuthHeader clientSettings.secretKey

        resource =
            clientSettings.vault ++ "/pan"
    in
        HttpBuilder.post resource
            |> withHeaders
                [ ( "Authorization", auth )
                , ( "Content-Type", "application/json" )
                , ( "Accept", "application/json" )
                ]
            |> HttpBuilder.withJsonBody body


create : Otp -> String -> ClientSettings -> Task.Task (Error String) (Response Pan)
create otp pan clientSettings =
    let
        body =
            Encode.object
                [ ( "otp", Encode.string otp.id )
                , ( "pan", Encode.string pan )
                ]

        successReader =
            HttpBuilder.jsonReader decodePan

        decodePan =
            Decode.object2 Pan
                ("id" := string)
                ("key" := string)
    in
        createRequest body clientSettings
            |> HttpBuilder.send successReader HttpBuilder.stringReader


createFakeRequest : Value -> ClientSettings -> RequestBuilder
createFakeRequest body clientSettings =
    let
        auth =
            buildAuthHeader clientSettings.secretKey

        resource =
            clientSettings.vault ++ "/pan/fake"
    in
        HttpBuilder.post resource
            |> withHeaders
                [ ( "Authorization", auth )
                , ( "Content-Type", "application/json" )
                , ( "Accept", "application/json" )
                ]
            |> HttpBuilder.withJsonBody body


createFake : String -> ClientSettings -> Task.Task (Error String) (Response FakePan)
createFake panId clientSettings =
    let
        body =
            Encode.object
                [ ( "panId", Encode.string panId )
                ]

        successReader =
            HttpBuilder.jsonReader decodePan

        decodePan =
            Decode.at [ "pan" ] string
    in
        createFakeRequest body clientSettings
            |> HttpBuilder.send successReader HttpBuilder.stringReader
