module Services.Job exposing (create)

import HttpBuilder exposing (Response, Error)
import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Encode as Encode
import Types exposing (ServiceApiConfig, Id, Value)
import Models exposing (Job)
import Task exposing (Task)
import Util exposing (buildAuthHeader)

create : ServiceApiConfig -> String -> Value -> Task (Error (List String)) (Response Job)
create apiConfig serviceId inputData =
    HttpBuilder.post (apiConfig.apiHost ++ "/jobs")
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
        



decodeJob : Decoder Job
decodeJob =
    Decode.object4 Job
        ("id" := Decode.string)
        ("state" := Decode.string)
        ("input" := Decode.value)
        ("output" := Decode.value)
        -- |: ("errors" := Decode.value)

