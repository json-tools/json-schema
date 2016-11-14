module Services.Otp exposing (create)

import Json.Decode exposing (Value)
import Types exposing (ClientSettings, RequestConfig)

create : Maybe Value -> ClientSettings -> RequestConfig
create body clientSettings =
    RequestConfig
        -- method : string
        "POST"
        -- baseurl : string
        clientSettings.vault
        -- pathname : string
        "/otp"
        -- auth : maybe string
        (Just clientSettings.secretKey)
        -- body : maybe value
        body

