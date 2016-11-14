module Services.Job exposing (create, createSchema, JobCreationError, JobCreationError(..))

import HttpBuilder exposing (Response, Error, RequestBuilder)
import Json.Decode as Decode exposing (Decoder, (:=), Value)
import Json.Encode as Encode
import JsonSchema as JS exposing (Schema)
import Types exposing (ClientSettings, Id, RequestConfig)
import Models exposing (Job, ValidationErrors)
import Task exposing (Task)
import Util exposing (buildAuthHeader, schema)
import String
import Dict

createSchema : Schema
createSchema =
    schema """
        { "type": "object"
        , "properties":
            { "serviceId":
                { "type": "string"
                , "format": "uuid"
                }
            , "input": { "type": "object" }
            }
        , "required": [ "serviceId", "input" ]
        }
    """

create : Maybe Value -> ClientSettings -> RequestConfig
create body clientSettings =
    RequestConfig
        "POST"
        clientSettings.service
        "/jobs"
        (Just clientSettings.secretKey)
        body


type JobCreationError
    = ValidationError ValidationErrors
    | HttpError (HttpBuilder.Error (List String))

createRequest : Value -> ClientSettings -> RequestBuilder
createRequest body clientSettings =
    HttpBuilder.post (clientSettings.service ++ "/jobs")
        |> HttpBuilder.withHeaders
           [ ( "Authorization", buildAuthHeader clientSettings.secretKey )
           , ( "Accept", "application/json" )
           ]
        |> HttpBuilder.withJsonBody body

create2 : ClientSettings -> String -> Value -> Task JobCreationError (Response Job)
create2 clientSettings serviceId inputData =
    let
        requestBody =
            Encode.object
                [ ( "service_id", Encode.string serviceId )
                , ( "input", inputData )
                ]

        sendRequest =
            HttpBuilder.post (clientSettings.service ++ "/jobs")
                |> HttpBuilder.withHeader "Authorization"
                    (buildAuthHeader clientSettings.secretKey)
                |> HttpBuilder.withJsonBody requestBody
                |> HttpBuilder.send
                    (HttpBuilder.jsonReader decodeJob)
                    (HttpBuilder.jsonReader decodeResponseError)

        decodeResponseError =
            Decode.at [ "details", "input" ] <|
                Decode.list Decode.string

        transformError err =
            case err of
                HttpBuilder.BadResponse resp ->
                    let
                        key str =
                            String.words str
                                |> List.head
                                |> Maybe.withDefault "."
                                |> String.dropLeft 1
                                |> String.split "."

                        val str =
                            String.words str
                                |> List.tail
                                |> Maybe.withDefault []
                                |> String.join " "

                        add str =
                            Dict.insert (key str) (val str)

                        validationErrors =
                            List.foldl add Dict.empty resp.data
                    in
                        Task.mapError ValidationError (Task.fail validationErrors)

                x -> Task.mapError HttpError (Task.fail x)
    in
        sendRequest `Task.onError` transformError




decodeJob : Decoder Job
decodeJob =
    Decode.object4 Job
        ("id" := Decode.string)
        ("state" := Decode.string)
        ("input" := Decode.value)
        ("output" := Decode.value)
        -- ("errors" := Decode.value)

