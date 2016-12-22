port module Main exposing (..)

import Types exposing (..)
import Html exposing (div, span, button, text, form, input, ul, li, hr, a)
import Navigation exposing (programWithFlags)
import Html.Attributes as Attrs exposing (style, href)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Json.Decode as Decode
import Http
import Messages exposing (Msg, Msg(..))
import Pages exposing (Page, Page(..))
import Pages.Settings
import Pages.Vault
import UrlParser exposing (Parser, (</>), int, oneOf, s, string)


main : Program ( PersistedData, Config, String ) Model Msg
main =
    programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port storeConfig : PersistedData -> Cmd msg


toHash : Page -> String
toHash page =
    case page of
        Settings ->
            "#settings"

        SecureVault ->
            "#secure-vault"

        AuditLog ->
            "#audit-log"



-- Blog id ->
--   "#blog/" ++ toString id
-- Search query ->
--   "#search/" ++ query


route : Parser (Page -> a) a
route =
    oneOf
        [ UrlParser.map Settings (s "settings")
        , UrlParser.map SecureVault (s "secure-vault")
        , UrlParser.map AuditLog (s "audit-log")
          --, map Blog (s "blog" </> int)
          --, map Search (s "search" </> string)
        ]



-- MODEL


type alias Model =
    { version : String
    , clientSettings : ClientSettings
    , vault : Pages.Vault.Model
    , history : List Page
    }


init : ( PersistedData, Config, String ) -> Navigation.Location -> ( Model, Cmd Msg )
init ( persistedData, conf, version ) location =
    let
        defaultSettings =
            ClientSettings
                "https://api.sandbox.automationcloud.net"
                "https://vault.sandbox.automationcloud.net"
                ""
                False

        cfg =
            case persistedData.clientSettings of
                Nothing ->
                    defaultSettings

                Just settings ->
                    settings
    in
        Model
            -- version
            version
            -- clientSettings
            cfg
            -- vault
            (Pages.Vault.init conf)
            -- history
            [ parsePath location ]
            ! []


parsePath : Navigation.Location -> Page
parsePath location =
    location
        |> UrlParser.parseHash route
        |> Maybe.withDefault SecureVault



-- ! [ fetchServices cfg ]
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --case Debug.log "update" msg of
    case msg of
        NoOp ->
            model ! []

        PagesSettingsMsg msg ->
            let
                settings =
                    Pages.Settings.update msg model.clientSettings
            in
                { model | clientSettings = settings }
                    ! [ storeConfig (PersistedData <| Just settings) ]

        PagesVaultMsg msg ->
            let
                ( vault, cmd ) =
                    Pages.Vault.update msg model.vault model.clientSettings
            in
                ( { model | vault = vault }, Cmd.map PagesVaultMsg cmd )

        NewUrl url ->
            ( model
            , Navigation.newUrl url
            )

        UrlChange location ->
            ( { model | history = parsePath location :: model.history }
            , Cmd.none
            )

        SetAuth str ->
            let
                cs =
                    model.clientSettings

                settings =
                    { cs | secretKey = str }
            in
                { model | clientSettings = settings }
                    ! [ storeConfig (PersistedData <| Just settings) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.none
        , Sub.none
        ]



-- VIEW


centerStyle : String -> String -> List ( String, String )
centerStyle direction align =
    [ ( "display", "flex" )
    , ( "flex-direction", direction )
    , ( "align-items", align )
    , ( "justify-content", "center" )
    ]


view : Model -> Html.Html Msg
view model =
    let
        currentPage =
            case model.history of
                head :: _ ->
                    toHash head

                _ ->
                    "#secure-vault"
    in
        div []
            [ div
                [ style
                    [ ( "background", "chocolate" )
                    , ( "color", "white" )
                    , ( "font-weight", "bold" )
                    , ( "padding", "10px" )
                    , ( "position", "relative" )
                    ]
                ]
                [ Html.label []
                    [ Html.h1 [] [ text "Automation Cloud Test Console" ]
                    , text "Secret key: "
                    , Html.input
                        [ onInput SetAuth
                        , Attrs.value model.clientSettings.secretKey
                        , Attrs.size 48
                        , Attrs.name "secret-key"
                        , Attrs.autocomplete True
                        , style
                            [ ( "font-family", "iosevka, menlo, monospace" )
                            , ( "font-size", "14px" )
                            , ( "border", "1px solid rgb(252, 249, 244)" )
                            , ( "margin", "0 5px" )
                            , ( "padding", "2px 4px" )
                            , ( "background", "#fcf9f4" )
                            , ( "color", "saddlebrown" )
                            , ( "border-radius", "2px" )
                            ]
                        ]
                        []
                    , Html.span
                        [ style
                            [ ( "position", "absolute" )
                            , ( "right", "10px" )
                            , ( "top", "10px" )
                            , ( "color", "#ebdbb2" )
                            , ( "font-weight", "normal" )
                            ]
                        ]
                        [ text <| "v" ++ model.version ]
                    ]
                , div
                    [ style
                        [ ( "color", "#eee" )
                        , ( "font-weight", "normal" )
                        , ( "margin-top", "5px" )
                        ]
                    ]
                    [ text "This key is used to authenticate all API requests. Contact API administrator (in slack) to obtain your secret key." ]
                ]
            , div [ style <| centerStyle "column" "stretch" ]
                [ case model.history of
                    Settings :: h ->
                        Html.map PagesSettingsMsg <|
                            Pages.Settings.render model.clientSettings

                    SecureVault :: h ->
                        Html.map PagesVaultMsg <|
                            Pages.Vault.render model.vault model.clientSettings

                    AuditLog :: h ->
                        model.vault.auditLog
                            |> List.reverse
                            |> List.map viewLogEntry
                            |> Html.ol []

                    _ ->
                        text ""
                ]
            ]


viewLogEntry : LogEntry -> Html.Html msg
viewLogEntry logEntry =
    let
        logLine =
            case logEntry of
                LogRequest req ->
                    Html.pre []
                        [ text <|
                            "Request "
                                ++ req.definition.method
                                ++ " "
                                ++ (if req.definition.service == "vault" then
                                        req.clientSettings.vault ++ req.definition.pathname
                                    else
                                        req.clientSettings.service ++ req.definition.pathname
                                   )
                                ++ " "
                                ++ (Maybe.withDefault Encode.null req.data |> Encode.encode 4)
                        ]

                LogResponse res ->
                    Html.pre
                        [ style
                            [ ( "max-height", "300px" )
                            , ( "overflow", "auto" )
                            , ( "background", "#eee" )
                            , ( "padding", "10px" )
                            ]
                        ]
                        [ text <|
                            "Response "
                                ++ (toString res.status)
                                ++ " "
                                ++ (res.body
                                        |> Decode.decodeString Decode.value
                                        |> Result.withDefault Encode.null
                                        |> Encode.encode 4
                                   )
                        ]

                LogError err ->
                    case err of
                        Http.BadStatus e ->
                            Html.pre []
                                [ text <|
                                    "ErrResponse "
                                        ++ (e.body
                                                |> Decode.decodeString Decode.value
                                                |> Result.withDefault Encode.null
                                                |> Encode.encode 4
                                           )
                                ]

                        _ ->
                            div [] [ text <| "ErrResponse " ++ (toString err) ]
    in
        Html.li [] [ logLine ]


viewLink : String -> String -> String -> Html.Html Msg
viewLink currentPage page description =
    let
        linkStyle =
            style [ ( "padding", "0 20px" ) ]

        txt =
            [ text description ]

        block =
            if currentPage == page then
                span [ linkStyle ]
            else
                a
                    [ style
                        [ ( "padding", "0 20px" )
                        , ( "color", "royalblue" )
                        , ( "cursor", "pointer" )
                        ]
                    , onClick (NewUrl page)
                    ]
    in
        block txt
