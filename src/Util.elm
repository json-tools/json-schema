module Util exposing (fetch, buildAuthHeader)

import HttpBuilder exposing (Error, Response)
import Task exposing (Task)
import Base64
import Types exposing (ServiceApiConfig)
import Json.Decode exposing (Decoder)


fetch : String -> Decoder a -> ServiceApiConfig -> Task (Error String) (Response a)
fetch url decoder apiConfig =
    let
        auth =
            buildAuthHeader apiConfig.clientSecretKey

        resource =
            apiConfig.apiHost ++ url

        successReader =
            HttpBuilder.jsonReader decoder
    in
        HttpBuilder.get resource
            |> HttpBuilder.withHeader "Authorization" auth
            |> HttpBuilder.send successReader HttpBuilder.stringReader


buildAuthHeader : String -> String
buildAuthHeader clientSecretKey =
    clientSecretKey
        ++ ":"
        |> Base64.encode
        |> Result.withDefault ""
        |> (++) "Basic "
