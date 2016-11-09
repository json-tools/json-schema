module Pages.Settings exposing (render, update, Msg)

import Layout exposing (boxStyle)
import Types exposing (..)
import Html exposing (div, span, button, text, form, input, ul, li, hr)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style)


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



render : ClientSettings -> Html.Html Msg
render clientSettings =
    let
        inputStyle =
            [ ( "width", "400px" )
            , ( "font-family", "menlo, monospace" )
            , ( "font-size", "12px" )
            ]

        labelStyle =
            [ ("margin-top", "10px")
            , ("margin-bottom", "5px")
            , ("font-weight", "700")
            ]
    in
        div [ style boxStyle ]
            [ div [ style labelStyle ] [ text "Service API url" ]
            , input
                [ Attrs.value clientSettings.service
                , Attrs.autocomplete True
                , Attrs.placeholder "Service API url"
                , Attrs.name "api-host"
                , Attrs.type' "url"
                , onInput SetServiceApiUrl
                , style inputStyle
                ] []
            , div [ style labelStyle ] [ text "Vault API (frontend) url" ]
            , input
                [ Attrs.value clientSettings.vault
                , Attrs.autocomplete True
                , Attrs.placeholder "Vault API url"
                , Attrs.name "vault url"
                , Attrs.type' "url"
                , onInput SetVaultApiUrl
                , style inputStyle
                ] []
            , div [ style labelStyle ] [ text "Client secret key" ]
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
