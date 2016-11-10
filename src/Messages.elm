module Messages exposing (Msg, Msg(..))

import Types exposing (Id, Value)
import Models exposing (Job, ServiceDescriptor, Context, Otp, Pan, FakePan)
import Pages.Settings
import Pages.Schema
import Pages.Vault
import HttpBuilder exposing (Response, Error)

type Msg
    = NoOp
    | PagesSettingsMsg Pages.Settings.Msg
    | PagesSchemaMsg Pages.Schema.Msg
    | PagesVaultMsg Pages.Vault.Msg
    | FetchServices
    | FetchServicesSuccess (Response (List ServiceDescriptor))
    | FetchService Id
    | FetchServiceSuccess (Response ServiceDescriptor)
    | ResponseError (Error String)
