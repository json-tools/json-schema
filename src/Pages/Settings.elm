module Pages.Settings exposing (render, update, Msg)

import String exposing (isEmpty)
import Layout exposing (boxStyle)
import Types exposing (..)
import Html exposing (div, span, button, text, form, input, ul, li)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style)


type Msg
    = SetClientSecretKey String
    | SetApiHost String
    | OnConfigured


update : Msg -> ServiceApiConfig -> ( ServiceApiConfig, Bool )
update msg model =
    case msg of
        SetClientSecretKey c ->
            ( { model | clientSecretKey = c }, False )

        SetApiHost c ->
            ( { model | apiHost = c }, False )

        OnConfigured ->
            ( model, True )


render : ServiceApiConfig -> Html.Html Msg
render apiConfig =
    form [ style boxStyle, onSubmit OnConfigured ]
        [ input
            [ Attrs.value apiConfig.apiHost
            , Attrs.autocomplete True
            , Attrs.placeholder "Service API url"
            , Attrs.name "api-host"
            , Attrs.type' "url"
            , onInput SetApiHost
            , style
                [ ( "width", "200px" )
                , ( "font-family", "menlo, monospace" )
                , ( "font-size", "12px" )
                ]
            ]
            []
        , input
            [ Attrs.value apiConfig.clientSecretKey
            , Attrs.autocomplete False
            , Attrs.placeholder "Client secret key (go grab it from db)"
            , Attrs.name "client-secret-key"
            , onInput SetClientSecretKey
            , style
                [ ( "width", "300px" )
                , ( "font-family", "menlo, monospace" )
                , ( "font-size", "12px" )
                ]
            ]
            []
        , button
            [ Attrs.type' "submit"
            , Attrs.disabled
                (isEmpty apiConfig.clientSecretKey)
            ]
            [ text "Fetch services" ]
        ]
