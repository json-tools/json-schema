module Util exposing (buildAuthHeader, performRequest, buildHeaders)

import Http exposing (Error, Response, Request, Header, header)
import Vendor.Base64 as Base64
import Types exposing (ClientSettings, RequestConfig, ApiEndpointDefinition)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode exposing (null)
import String
import Regex
import Dict



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
            interpolate req.pathname body

        paramsMatcher =
            Regex.regex ":\\w+"

        interpolate str val =
            Regex.replace Regex.All
                paramsMatcher
                (\{ match } ->
                    (Maybe.withDefault null val)
                        |> Decode.decodeValue (Decode.at [ String.dropLeft 1 match ] Decode.string)
                        |> Result.withDefault ""
                )
                str

        objectBody =
            body
                |> Maybe.withDefault null
                |> Decode.decodeValue (Decode.keyValuePairs Decode.value)
                |> Result.withDefault []
                |> Dict.fromList

        processedBody =
            req.pathname
                |> Debug.log "pathName"
                |> Regex.find Regex.All paramsMatcher
                |> List.map .match
                |> List.map (String.dropLeft 1)
                |> List.foldl Dict.remove objectBody
                |> Dict.toList
                |> Encode.object
                |> Debug.log "processedBody"

    in
        Http.request
            { method = method
            , headers = buildHeaders req clientSettings |> List.map (\(k, v) -> header k v)
            , url = serviceUrl ++ pathname
            , body = Http.jsonBody <| processedBody
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
