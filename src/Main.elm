port module Main exposing (..)

import Types exposing (..)
import Html exposing (div, span, button, text, form, input, ul, li, hr, a)
import Navigation exposing (programWithFlags)
import Html.App
import Html.Attributes as Attrs exposing (style, href)
import String
import Messages exposing (Msg, Msg(..))
import Pages exposing (Page, Page(..))
import Pages.Settings
import Pages.Vault
import Pages.ServiceApi
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)


main : Program (PersistedData)
main =
    programWithFlags (Navigation.makeParser hashParser)
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
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

        ServiceApi ->
            "#service-api"



-- Blog id ->
--   "#blog/" ++ toString id
-- Search query ->
--   "#search/" ++ query


hashParser : Navigation.Location -> Result String Page
hashParser location =
    UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format Home (s "home")
        , format Settings (s "settings")
        , format SecureVault (s "secure-vault")
        , format ServiceApi (s "service-api")
          --, format Blog (s "blog" </> int)
          --, format Search (s "search" </> string)
        ]


{-| The URL is turned into a result. If the URL is valid, we just update our
model to the new count. If it is not a valid URL, we modify the URL to make
sense.
-}
urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case Debug.log "result" result of
        Err _ ->
            ( model, Navigation.modifyUrl (toHash model.page) )

        Ok page ->
            { model | page = page } ! []



{-
   Ok ((Search query) as page) ->
       { model
           | page = page
           , query = query
       }
           ! if Dict.member query model.cache then
               []
             else
               [ get query ]

   Ok page ->
       { model
           | page = page
           , query = ""
       }
           ! []
-}
-- MODEL


type alias Model =
    { page : Page
    , clientSettings : ClientSettings
    , vault : Pages.Vault.Model
    , serviceApi : Pages.ServiceApi.Model
    }


init : PersistedData -> Result String Page -> ( Model, Cmd Msg )
init persistedData result =
    let
        defaultSettings =
            ClientSettings
                "http://localhost:3000"
                "https://localhost:5000"
                ""
                False

        cfg =
            case persistedData.clientSettings of
                Nothing ->
                    defaultSettings

                Just settings ->
                    settings
    in
        urlUpdate result <|
            (Model
                -- page
                Settings
                -- clientSettings
                cfg
                -- vault
                Pages.Vault.init
                -- serviceApi
                Pages.ServiceApi.init
            )



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
                { model | clientSettings = settings } !
                    [ storeConfig (PersistedData <| Just settings) ]

        PagesServiceApiMsg msg ->
            let
                ( serviceApi, cmd ) =
                    Pages.ServiceApi.update msg model.serviceApi
            in
                ( { model | serviceApi = serviceApi }, Cmd.map PagesServiceApiMsg cmd )

        PagesVaultMsg msg ->
            let
                ( vault, cmd ) =
                    Pages.Vault.update msg model.vault model.clientSettings
            in
                ( { model | vault = vault }, Cmd.map PagesVaultMsg cmd )





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
    div []
        [ div [ centerStyle "row" "center" ]
            [ viewLink model.page Home "Home"
            , viewLink model.page Settings "Settings"
            , viewLink model.page SecureVault "Secure Vault"
            , viewLink model.page ServiceApi "Service API"
            ]
        , hr [ style [ ( "border", "0" ), ("border-bottom", "1px solid #ddd" ) ] ] []
        , div [ centerStyle "column" "stretch" ]
            (case model.page of
                Home ->
                    [ text "Welcome to Automation Cloud Test Client" ]

                Settings ->
                    [ Html.App.map PagesSettingsMsg <|
                        Pages.Settings.render model.clientSettings
                    ]

                SecureVault ->
                    [ Html.App.map PagesVaultMsg <|
                        Pages.Vault.render model.vault model.clientSettings
                    ]

                ServiceApi ->
                    [ Html.App.map PagesServiceApiMsg <|
                        Pages.ServiceApi.render model.serviceApi ]
            )
        ]


viewLink : Page -> Page -> String -> Html.Html msg
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
            a [ linkStyle, href (toHash page) ]
    in
        block txt


