module Pages.Settings exposing (render)

import String exposing (isEmpty)
import Layout exposing (boxStyle)
import Messages exposing (Msg, Msg(..))
import Types exposing (..)
import Html exposing (div, span, button, text, form, input, ul, li)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style)

render : ServiceApiConfig -> Html.Html Msg
render apiConfig =
    form [ style boxStyle, onSubmit FetchServices ]
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
