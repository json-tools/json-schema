module Services.Pan exposing (create, createSchema, createFake, createFakeSchema)

import JsonSchema as JS exposing (Schema)
import Json.Decode as Decode exposing (Decoder, string, (:=), Value)
import Types exposing (ClientSettings, RequestConfig)
import Util exposing (buildAuthHeader, schema)


create : Maybe Value -> ClientSettings -> RequestConfig
create body clientSettings =
    RequestConfig
        -- method : string
        "POST"

        -- baseurl : string
        clientSettings.vault

        -- pathname : string
        "/pan"

        -- auth : this is unauthenticated endpoint
        Nothing

        -- body : maybe value
        body

createSchema : Schema
createSchema =
    schema """
        { "type": "object"
        , "properties":
            { "otp":
                { "type": "string"
                , "format": "uuid"
                }
            , "pan":
                { "type": "string"
                }
            }
        , "required": [ "pan", "otp" ]
        }
    """


createFake: Maybe Value -> ClientSettings -> RequestConfig
createFake body clientSettings =
    RequestConfig
        -- method : string
        "POST"

        -- baseurl : string
        clientSettings.vault

        -- pathname : string
        "/pan/fake"

        -- auth : maybe string
        (Just clientSettings.secretKey)

        -- body : maybe value
        body

createFakeSchema : Schema
createFakeSchema =
    schema """
        { "type": "object"
        , "properties":
            { "panId":
                { "type": "string"
                , "format": "uuid"
                }
            }
        , "required": [ "panId" ]
        }
    """

