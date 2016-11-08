module Messages exposing (Msg, Msg(..))

import Types exposing (Id, Value)
import Models exposing (Job, ServiceDescriptor, Context, ValidationErrors)
import HttpBuilder

type Msg
    = NoOp
    | SetClientSecretKey String
    | SetApiHost String
    | FetchServices
    | FetchServicesSuccess (HttpBuilder.Response (List ServiceDescriptor))
    | FetchService Id
    | FetchServiceSuccess (HttpBuilder.Response ServiceDescriptor)
    | UpdateProperty Context (List String) Value
    | SubmitJob
    | SubmitJobError (HttpBuilder.Error ValidationErrors)
    | SubmitJobSuccess (HttpBuilder.Response Job)
    | ResponseError (HttpBuilder.Error String)

