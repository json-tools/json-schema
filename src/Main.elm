port module Main exposing (..)

import Types exposing (..)
import Html exposing (div, span, button, text, form, input, ul, li, hr, a)
import Navigation exposing (programWithFlags)
import Html.Attributes as Attrs exposing (style, href)
import Html.Events exposing (onClick)
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
                "https://api.staging.automationcloud.net"
                "https://vault.staging.automationcloud.net"
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
        |> Maybe.withDefault Home



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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.none
        ]



-- VIEW


centerStyle : String -> String -> Html.Attribute msg
centerStyle direction align =
    style
        [ ( "display", "flex" )
        , ( "flex-direction", direction )
        , ( "align-items", align )
        , ( "justify-content", "center" )
        , ( "padding", "20px 0" )
        ]


view : Model -> Html.Html Msg
view model =
    let
        currentPage =
            case model.history of
                head :: _ ->
                    toHash head

                _ ->
                    "#home"
    in
        div []
            [ div [ centerStyle "row" "center" ]
                [ viewLink currentPage "#home" "Home"
                , viewLink currentPage "#settings" "Settings"
                , viewLink currentPage "#secure-vault" "Integration Example"
                  -- , viewLink model.page ServiceApi "Service API"
                ]
            , hr [ style [ ( "border", "0" ), ( "border-bottom", "1px solid #ddd" ) ] ] []
            , div [ centerStyle "column" "stretch" ]
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
