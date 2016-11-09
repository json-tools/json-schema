module Util exposing (fetch, buildAuthHeader)

import HttpBuilder exposing (Error, Response)
import Task exposing (Task)
import Base64
import Types exposing (ClientSettings)
import Json.Decode exposing (Decoder)


fetch : String -> Decoder a -> ClientSettings -> Task (Error String) (Response a)
fetch url decoder clientSettings =
    let
        auth =
            buildAuthHeader clientSettings.secretKey

        resource =
            clientSettings.service ++ url

        successReader =
            HttpBuilder.jsonReader decoder
    in
        HttpBuilder.get resource
            |> HttpBuilder.withHeader "Authorization" auth
            |> HttpBuilder.send successReader HttpBuilder.stringReader

buildAuthHeader : String -> String
buildAuthHeader secretKey =
    secretKey
        ++ ":"
        |> Base64.encode
        |> Result.withDefault ""
        |> (++) "Basic "
