port module Main exposing (..)

import Models exposing (..)
import Types exposing (..)
import Html exposing (div, span, button, text, form, input, ul, li)
import Html.App exposing (programWithFlags)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style)
import Task
import Dict
import JsonSchema as JS
import Services.ServiceDescriptor as ServiceDescriptorSvc
import Messages exposing (Msg, Msg(..))
import Pages.Settings
import Pages.Schema
import Layout exposing (boxStyle)


main : Program (Maybe PersistedData)
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port storeConfig : PersistedData -> Cmd msg



-- MODEL


type alias Model =
    { services : Maybe (List ServiceDescriptor)
    , error : String
    , validationErrors : ValidationErrors
    , apiConfig : ServiceApiConfig
    , schema : Maybe Schema
    , input : Maybe Value
    , serviceId : Id
    , job : Maybe Job
    }


init : Maybe PersistedData -> ( Model, Cmd Msg )
init persistedData =
    let
        cfg =
            case persistedData of
                Nothing ->
                    ServiceApiConfig "http://localhost:3000" ""

                Just pd ->
                    pd.serviceApi
    in
        Model
            -- list services
            Nothing
            -- error
            ""
            -- validationErrors
            Dict.empty
            -- apiConfig
            cfg
            -- schema
            Nothing
            -- input
            Nothing
            -- serviceId
            ""
            -- job
            Nothing
            ! [ fetchServices cfg ]


fetchServices : ServiceApiConfig -> Cmd Msg
fetchServices cfg =
    Task.perform ResponseError FetchServicesSuccess <|
        ServiceDescriptorSvc.list cfg


fetchService : Id -> ServiceApiConfig -> Cmd Msg
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
                ( settings, isConfigured ) =
                    Pages.Settings.update msg model.apiConfig
            in
                { model | apiConfig = settings }
                    ! [ storeConfig (PersistedData settings)
                      , if isConfigured then
                            fetchServices settings
                        else
                            Cmd.none
                      ]

        PagesSchemaMsg msg ->
            let
                ( model, cmd ) =
                    Pages.Schema.update msg model
            in
                ( model, Cmd.map PagesSchemaMsg cmd )

        FetchServices ->
            { model | error = "" } ! [ fetchServices model.apiConfig ]

        ResponseError err ->
            { model | error = toString err } ! []

        FetchServicesSuccess { data } ->
            { model | services = Just data } ! []

        FetchService id ->
            { model | serviceId = id, error = "" } ! [ fetchService id model.apiConfig ]

        FetchServiceSuccess { data } ->
            case JS.convert data.schema of
                Ok s ->
                    { model | schema = Just s } ! []

                Err err ->
                    { model | error = err } ! []





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


view : Model -> Html.Html Msg
view model =
    let
        credentials =
            Html.App.map PagesSettingsMsg <| Pages.Settings.render model.apiConfig

        services =
            case model.services of
                Nothing ->
                    text ""

                Just svcs ->
                    renderServices svcs model.serviceId

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
            [ credentials
            , services
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
        div [ style boxStyle ]
            [ div [] <| List.map renderService services ]
