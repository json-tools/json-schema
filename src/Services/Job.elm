module Services.Job exposing (create)

import HttpBuilder exposing (Response, Error)
import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Encode as Encode
import Types exposing (ServiceApiConfig, Id, Value)
import Models exposing (Job, ValidationErrors)
import Task exposing (Task)
import Util exposing (buildAuthHeader)
import String
import Dict

create : ServiceApiConfig -> String -> Value -> Task (Error ValidationErrors) (Response Job)
create apiConfig serviceId inputData =
    (HttpBuilder.post (apiConfig.apiHost ++ "/jobs")
        |> HttpBuilder.withHeader "Authorization"
            (buildAuthHeader apiConfig.clientSecretKey)
        |> HttpBuilder.withJsonBody
            (Encode.object
                [ ( "service_id", Encode.string serviceId )
                , ( "input", inputData )
                ]
            )
        |> HttpBuilder.send
            (HttpBuilder.jsonReader decodeJob)
            (HttpBuilder.jsonReader <|
                Decode.at [ "details", "input" ] <|
                    Decode.list Decode.string
            )
    )
        |> Task.mapError (\err ->
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

                        newData =
                            List.foldl add Dict.empty resp.data
                    in
                        Task.fail (HttpBuilder.BadResponse { resp | data = newData } )

                _ ->
                    err
            )




decodeJob : Decoder Job
decodeJob =
    Decode.object4 Job
        ("id" := Decode.string)
        ("state" := Decode.string)
        ("input" := Decode.value)
        ("output" := Decode.value)
        -- |: ("errors" := Decode.value)

