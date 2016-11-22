module Messages exposing (Msg, Msg(..))

import Pages.Settings
import Navigation


-- import Pages.ServiceApi

import Pages.Vault


type Msg
    = NoOp
    | PagesSettingsMsg Pages.Settings.Msg
      --| PagesServiceApiMsg Pages.ServiceApi.Msg
    | PagesVaultMsg Pages.Vault.Msg
    | UrlChange Navigation.Location
    | NewUrl String
    | SetAuth String
