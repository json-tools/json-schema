module Pages.Settings exposing (render, update, Msg)

import Layout exposing (boxStyle)
import Types exposing (..)
import Html exposing (div, span, button, text, form, input, ul, li, hr)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style)
import Markdown


type Msg
    = SetClientSecretKey String
    | SetServiceApiUrl String
    | SetVaultApiUrl String


update : Msg -> ClientSettings -> ClientSettings
update msg model =
    case msg of
        SetClientSecretKey c ->
            { model | secretKey = c }

        SetServiceApiUrl c ->
            { model | service = c }

        SetVaultApiUrl c ->
            { model | vault = c }


service : String
service =
    """
## Service API

This API provides automation-related endpoints:

- list services `GET /services` -> List Service
- get schema for specific service `GET /service/:id` -> Service
- create automation job `POST /jobs` -> Job
- poll state of specific job `GET /jobs/:id` -> Job
"""


vault : String
vault =
    """
## Secure Vault

PCI Compliant secure storage which allows to store PAN.
Following endpoints open for public use:

- create OTP `POST /otp`
- store PAN `POST /pan {otp}`
- issue fake PAN `POST /pan/fake`
"""

auth : String
auth =
    """
## Authentication

All API services require authentication via Basic HTTP auth over HTTPS.
Unauthorised requests will result in 401 Unauthorized.
All requests using the HTTP protocol will fail with 403 Forbidden status code.
"""


render : ClientSettings -> Html.Html Msg
render clientSettings =
    let
        inputStyle =
            [ ( "width", "400px" )
            , ( "font-family", "menlo, monospace" )
            , ( "font-size", "12px" )
            ]

        labelStyle =
            [ ( "margin-top", "10px" )
            , ( "margin-bottom", "5px" )
            , ( "font-weight", "700" )
            ]
    in
        div []
            [ div [ style boxStyle ]
                [ Markdown.toHtml [] service
                , input
                    [ Attrs.value clientSettings.service
                    , Attrs.autocomplete True
                    , Attrs.placeholder "Service API url"
                    , Attrs.name "api-host"
                    , Attrs.type' "url"
                    , onInput SetServiceApiUrl
                    , style inputStyle
                    ]
                    []
                ]
            , div [ style boxStyle ]
                [ Markdown.toHtml [] vault
                , input
                    [ Attrs.value clientSettings.vault
                    , Attrs.autocomplete True
                    , Attrs.placeholder "Vault API url"
                    , Attrs.name "vault url"
                    , Attrs.type' "url"
                    , onInput SetVaultApiUrl
                    , style inputStyle
                    ]
                    []
                ]
            , div [ style boxStyle ]
                [ Markdown.toHtml [] auth
                , input
                    [ Attrs.value clientSettings.secretKey
                    , Attrs.autocomplete False
                    , Attrs.placeholder "Client secret key (go grab it from db)"
                    , Attrs.name "client-secret-key"
                    , onInput SetClientSecretKey
                    , style inputStyle
                    ]
                    []
                ]
            ]
