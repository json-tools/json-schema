module Util exposing (fetch, buildAuthHeader, performRequest, schema, buildHeaders)

import HttpBuilder exposing (Error, Response, jsonReader, RequestBuilder)
import Task exposing (Task)
import Base64
import Types exposing (ClientSettings, RequestConfig)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode exposing (null)
import JsonSchema as JS exposing (Schema)
import String

schema : String -> Schema
schema str =
    case JS.fromString str of
        Err e ->
            JS.empty
                |> Debug.log ("Can not parse schema" ++ str)
        Ok s ->
            s

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

buildHeaders : RequestConfig -> List (String, String)
buildHeaders req =
    let
        contentTypeHeader h =
            case String.toUpper req.method of
                "GET" ->
                    h

                "DELETE" ->
                    h

                _ ->
                    ( "Content-Type", "application/json" ) :: h

        authHeader h =
            case req.auth of
                Nothing ->
                    h

                Just auth ->
                    ( "Authorization", buildAuthHeader auth ) :: h

    in
        [ ( "Accept", "application/json" ) ]
            |> authHeader
            |> contentTypeHeader

performRequest : RequestConfig -> Task (Error Value) (Response Value)
performRequest req =
    let
        json =
            jsonReader Decode.value

        method =
            case req.method of
                "POST" ->
                    HttpBuilder.post

                "GET" ->
                    HttpBuilder.get

                "DELETE" ->
                    HttpBuilder.delete

                _ ->
                    HttpBuilder.put
    in
        method (req.baseUrl ++ req.pathname)
            |> HttpBuilder.withHeaders (buildHeaders req)
            |> HttpBuilder.withJsonBody (Maybe.withDefault null req.body)
            |> HttpBuilder.send json json


buildAuthHeader : String -> String
buildAuthHeader secretKey =
    secretKey
        ++ ":"
        |> Base64.encode
        |> Result.withDefault ""
        |> (++) "Basic "
