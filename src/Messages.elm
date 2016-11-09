module Messages exposing (Msg, Msg(..))

import Types exposing (Id, Value)
import Models exposing (Job, ServiceDescriptor, Context)
import Pages.Settings
import Pages.Schema
import HttpBuilder

type Msg
    = NoOp
    | PagesSettingsMsg Pages.Settings.Msg
    | PagesSchemaMsg Pages.Schema.Msg
    | FetchServices
    | FetchServicesSuccess (HttpBuilder.Response (List ServiceDescriptor))
    | FetchService Id
    | FetchServiceSuccess (HttpBuilder.Response ServiceDescriptor)
    | ResponseError (HttpBuilder.Error String)

