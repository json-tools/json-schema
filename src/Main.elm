port module Main exposing (..)

import Models exposing (..)
import Types exposing (..)
import Html exposing (div, span, button, text, form, input, ul, li, hr, a)
import Navigation exposing (programWithFlags)
import Html.App
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style, href)
import Task
import Dict
import String
import JsonSchema as JS
import Services.ServiceDescriptor as ServiceDescriptorSvc
import Services.Otp as OtpSvc
import Messages exposing (Msg, Msg(..))
import Pages exposing (Page, Page(..))
import Pages.Settings
import Pages.Schema
import Layout exposing (boxStyle)
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
    , services : Maybe (List ServiceDescriptor)
    , error : String
    , validationErrors : ValidationErrors
    , clientSettings : ClientSettings
    , schema : Maybe Schema
    , input : Maybe Value
    , serviceId : Id
    , job : Maybe Job
    , otp : Maybe Otp
    }


init : PersistedData -> Result String Page -> ( Model, Cmd Msg )
init persistedData result =
    let
        defaultSettings =
            ClientSettings
                "http://localhost:3000"
                "https://localhost:5000"
                ""

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
                -- list services
                Nothing
                -- error
                ""
                -- validationErrors
                Dict.empty
                -- clientSettings
                cfg
                -- schema
                Nothing
                -- input
                Nothing
                -- serviceId
                ""
                -- job
                Nothing
                -- otp
                Nothing
            )



-- ! [ fetchServices cfg ]


fetchServices : ClientSettings -> Cmd Msg
fetchServices cfg =
    Task.perform ResponseError FetchServicesSuccess <|
        ServiceDescriptorSvc.list cfg


fetchService : Id -> ClientSettings -> Cmd Msg
fetchService id cfg =
    Task.perform ResponseError FetchServiceSuccess <|
        ServiceDescriptorSvc.get id cfg



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
                { model | clientSettings = settings } ! []

        PagesSchemaMsg msg ->
            let
                ( model, cmd ) =
                    Pages.Schema.update msg model
            in
                ( model, Cmd.map PagesSchemaMsg cmd )

        FetchServices ->
            { model | error = "" } ! [ fetchServices model.clientSettings ]

        ResponseError err ->
            { model | error = toString err } ! []

        FetchServicesSuccess { data } ->
            { model | services = Just data } ! []

        FetchService id ->
            { model | serviceId = id, error = "" } ! [ fetchService id model.clientSettings ]

        FetchServiceSuccess { data } ->
            case JS.convert data.schema of
                Ok s ->
                    { model | schema = Just s } ! []

                Err err ->
                    { model | error = err } ! []

        CreateOtp ->
            model ! [ Task.perform ResponseError CreateOtpSuccess <| OtpSvc.create model.clientSettings ]

        CreateOtpSuccess { data } ->
            { model | otp = Just data } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.none
        ]



-- VIEW


entityRowStyle : List ( String, String )
entityRowStyle =
    [ ( "padding", "5px" )
    , ( "background", "#eee" )
    , ( "margin-top", "5px" )
    , ( "cursor", "pointer" )
    , ( "font-family", "menlo, monospace" )
    ]


centerStyle : String -> Html.Attribute msg
centerStyle direction =
    style
        [ ( "display", "flex" )
        , ( "flex-direction", direction )
        , ( "align-items", "center" )
        , ( "justify-content", "center" )
        , ( "padding", "20px 0" )
        ]


view : Model -> Html.Html Msg
view model =
    div []
        [ div [ centerStyle "row" ]
            [ viewLink Home "Home"
            , viewLink Settings "Settings"
            , viewLink SecureVault "Secure Vault"
            , viewLink ServiceApi "Service API"
            ]
        , hr [] []
        , div [ centerStyle "column" ]
            (case model.page of
                Home ->
                    [ text "Welcome to Automation Cloud Test Client" ]

                Settings ->
                    [ Html.App.map PagesSettingsMsg <|
                        Pages.Settings.render model.clientSettings
                    ]

                SecureVault ->
                    let
                        otpId =
                            case model.otp of
                                Nothing ->
                                    text ""

                                Just otp ->
                                    text otp.id
                    in
                        [ div [ style boxStyle ]
                            [ button [ onClick CreateOtp ] [ text "Create OTP" ]
                            , otpId
                            ]
                        ]

                ServiceApi ->
                    [ all model ]
            )
        ]


viewLink : Page -> String -> Html.Html msg
viewLink page description =
    a [ style [ ( "padding", "0 20px" ) ], href (toHash page) ] [ text description ]


all : Model -> Html.Html Msg
all model =
    let
        services =
            div [ style boxStyle ]
                [ button
                    [ Attrs.disabled
                        (String.isEmpty model.clientSettings.secretKey)
                    , onClick FetchServices
                    ]
                    [ text "Fetch services" ]
                , (case model.services of
                    Nothing ->
                        text ""

                    Just svcs ->
                        renderServices svcs model.serviceId
                  )
                ]

        schema =
            case model.schema of
                Nothing ->
                    text ""

                Just schema ->
                    let
                        context =
                            Context
                                schema
                                (Maybe.withDefault (JS.defaultFor schema) model.input)
                                model.validationErrors
                    in
                        Html.App.map PagesSchemaMsg <| Pages.Schema.render context schema

        job =
            case model.job of
                Nothing ->
                    text ""

                Just j ->
                    div [ style boxStyle ]
                        [ div [ style entityRowStyle ]
                            [ text ("Job " ++ j.id ++ ": " ++ j.state)
                            ]
                        ]
    in
        div []
            [ services
            , schema
            , job
            , text model.error
            ]


renderServices : List ServiceDescriptor -> Id -> Html.Html Msg
renderServices services id =
    let
        entityStyle isActive =
            [ ( "margin-right", "10px" )
            , ( "display", "inline-block" )
            , ( "font-weight", "bold" )
            , ( "background"
              , if isActive then
                    "black"
                else
                    "lightgrey"
              )
            , ( "color"
              , if isActive then
                    "lightyellow"
                else
                    "black"
              )
            ]

        renderService svc =
            span
                [ style <| (++) entityRowStyle <| entityStyle <| svc.id == id
                , onClick (FetchService svc.id)
                ]
                [ text svc.name ]
    in
        div [] <| List.map renderService services
