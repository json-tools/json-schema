module Util exposing (buildAuthHeader, performRequest, buildHeaders)

import Http exposing (Error, Response, Request, Header, header)
import Vendor.Base64 as Base64
import Types exposing (ClientSettings, RequestConfig, ApiEndpointDefinition)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode exposing (null)
import String
import Regex



buildHeaders : ApiEndpointDefinition -> ClientSettings -> List (String, String)
buildHeaders req clientSettings =
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
            if req.auth then
                ( "Authorization", buildAuthHeader clientSettings.secretKey ) :: h
            else
                h
    in
        [ ( "Accept", "application/json" ) ]
            |> authHeader
            |> contentTypeHeader


performRequest : ClientSettings -> Maybe Value -> ApiEndpointDefinition -> Request (Response String)
performRequest clientSettings body req =
    let

        serviceUrl =
            if req.service == "vault" then
                clientSettings.vault
            else
                clientSettings.service

        method =
            String.toUpper req.method

        pathname =
            case method of
                "GET" ->
                    interpolate req.pathname body

                "DELETE" ->
                    interpolate req.pathname body

                _ ->
                    req.pathname

        interpolate str val =
            Regex.replace Regex.All
                (Regex.regex ":\\w+")
                (\{ match } ->
                    (Maybe.withDefault null val)
                        |> Decode.decodeValue (Decode.at [ String.dropLeft 1 match ] Decode.string)
                        |> Result.withDefault ""
                )
                str
    in
        Http.request
            { method = method
            , headers = buildHeaders req clientSettings |> List.map (\(k, v) -> header k v)
            , url = serviceUrl ++ pathname
            , body = Http.jsonBody <| Maybe.withDefault null body
            , expect = Http.expectStringResponse (\r -> Ok r)
            , timeout = Nothing
            , withCredentials = False
            }


buildAuthHeader : String -> String
buildAuthHeader secretKey =
    secretKey
        ++ ":"
        |> Base64.encode
        |> Result.withDefault ""
        |> (++) "Basic "
