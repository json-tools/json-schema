module Messages exposing (Msg, Msg(..))

import Types exposing (Id, Value)
import Models exposing (Job, ServiceDescriptor, Context, Otp, Pan, FakePan)
import Pages.Settings
import Pages.ServiceApi
import Pages.Schema
import Pages.Vault

type Msg
    = NoOp
    | PagesSettingsMsg Pages.Settings.Msg
    | PagesServiceApiMsg Pages.ServiceApi.Msg
    | PagesSchemaMsg Pages.Schema.Msg
    | PagesVaultMsg Pages.Vault.Msg
