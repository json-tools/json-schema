module Messages exposing (Msg, Msg(..))

import Types exposing (Id, Value)
import Models exposing (Job, ServiceDescriptor)
import HttpBuilder

type Msg
    = NoOp
    | SetClientSecretKey String
    | SetApiHost String
    | FetchServices
    | FetchServicesSuccess (HttpBuilder.Response (List ServiceDescriptor))
    | FetchService Id
    | FetchServiceSuccess (HttpBuilder.Response ServiceDescriptor)
    | UpdateProperty (List String) Value
    | SubmitJob
    | SubmitJobError (HttpBuilder.Error (List String))
    | SubmitJobSuccess (HttpBuilder.Response Job)
    | ResponseError (HttpBuilder.Error String)

