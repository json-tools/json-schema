port module Main exposing (..)

import Types exposing (..)
import Html exposing (div, span, button, text, form, input, ul, li, hr, a)
import Navigation exposing (programWithFlags)
import Html.Attributes as Attrs exposing (style, href)
import Html.Events exposing (onClick, onInput)
import Messages exposing (Msg, Msg(..))
import Pages exposing (Page, Page(..))
import Pages.Settings
import Pages.Vault
import UrlParser exposing (Parser, (</>), int, oneOf, s, string)


main : Program ( PersistedData, Config ) Model Msg
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
        Home ->
            "#home"

        Settings ->
            "#settings"

        SecureVault ->
            "#secure-vault"



-- Blog id ->
--   "#blog/" ++ toString id
-- Search query ->
--   "#search/" ++ query


route : Parser (Page -> a) a
route =
    oneOf
        [ UrlParser.map Home (s "home")
        , UrlParser.map Settings (s "settings")
        , UrlParser.map SecureVault (s "secure-vault")
          --, map Blog (s "blog" </> int)
          --, map Search (s "search" </> string)
        ]



-- MODEL


type alias Model =
    { clientSettings : ClientSettings
    , vault : Pages.Vault.Model
    , history : List Page
    }


init : ( PersistedData, Config ) -> Navigation.Location -> ( Model, Cmd Msg )
init ( persistedData, conf ) location =
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
                (case model.history of
                    Home :: h ->
                        [ text "Welcome to Automation Cloud Test Client" ]

                    Settings :: h ->
                        [ Html.map PagesSettingsMsg <|
                            Pages.Settings.render model.clientSettings
                        ]

                    SecureVault :: h ->
                        [ Html.map PagesVaultMsg <|
                            Pages.Vault.render model.vault model.clientSettings
                        ]

                    _ ->
                        [ text "" ]
                )
            ]


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
