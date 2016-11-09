module Services.Job exposing (create, JobCreationError, JobCreationError(..))

import HttpBuilder exposing (Response, Error)
import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Encode as Encode
import Types exposing (ClientSettings, Id, Value)
import Models exposing (Job, ValidationErrors)
import Task exposing (Task)
import Util exposing (buildAuthHeader)
import String
import Dict

type JobCreationError
    = ValidationError ValidationErrors
    | HttpError (HttpBuilder.Error (List String))

create : ClientSettings -> String -> Value -> Task JobCreationError (Response Job)
create clientSettings serviceId inputData =
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

