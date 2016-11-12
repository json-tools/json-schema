module Pages.ServiceApi exposing (Msg, update, Model, init, render)

import Services.Job as JobSvc exposing (JobCreationError)
import Services.ServiceDescriptor as ServiceDescriptorSvc
import HttpBuilder exposing (Response, Error)
import Task
import Html exposing (span, div, button, text, form)
import Html.App
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style)
import Layout exposing (boxStyle)
import Models exposing (Job, ServiceDescriptor)
import Pages.Schema
import Types exposing (ClientSettings, Id)
import JsonSchema as JS exposing (convert)

type alias Model =
    { jobForm : Pages.Schema.Model
    , serviceId : String
    , job : Maybe Job
    , error : String
    , services : List ServiceDescriptor
    }

init : Model
init =
    Model
        -- jobForm
        Pages.Schema.init
        -- serviceId
        ""
        -- job
        Nothing
        -- error
        ""
        -- services
        []

type Msg
    = SubmitJob
    | SubmitJobError JobCreationError
    | SubmitJobSuccess (HttpBuilder.Response Job)
    | PagesSchemaMsg Pages.Schema.Msg
    | FetchServices
    | FetchServicesSuccess (Response (List ServiceDescriptor))
    | FetchService Id
    | FetchServiceSuccess (Response ServiceDescriptor)
    | ResponseError (Error String)

update : Msg -> Model -> ClientSettings -> ( Model, Cmd Msg )
update msg model clientSettings =
    case msg of
        PagesSchemaMsg msg ->
            let
                ( updatedSubmodel, cmd ) =
                    Pages.Schema.update msg model.jobForm
            in
                ( { model | jobForm = updatedSubmodel }, Cmd.map PagesSchemaMsg cmd )

        SubmitJob ->
            { model | error = "" }
                ! [ Task.perform SubmitJobError SubmitJobSuccess <|
                        JobSvc.create clientSettings model.serviceId model.jobForm.data
                  ]


        SubmitJobSuccess { data } ->
            { model | job = Just data } ! []

        SubmitJobError err ->
            case err of
                JobSvc.ValidationError errors ->
                    let
                        form f =
                            { f | validationErrors = errors }
                    in
                        { model | jobForm = form model.jobForm } ! []

                e ->
                    { model | error = toString e } ! []

        FetchServices ->
            { model | error = "" } ! [ fetchServices clientSettings ]

        ResponseError err ->
            { model | error = toString err } ! []

        FetchServicesSuccess { data } ->
            { model | services = data } ! []

        FetchService id ->
            { model | serviceId = id } !
                [ fetchService id clientSettings ]

        FetchServiceSuccess { data } ->
            case JS.convert data.schema of
                Ok s ->
                    let
                        jobForm f =
                            { f | schema = s }
                    in
                        { model | jobForm = jobForm model.jobForm } ! []

                Err err ->
                    { model | error = err } ! []

render : Model -> Html.Html Msg
render model =
    let
        services =
            div [ style boxStyle ]
                [ button
                    [ onClick FetchServices ]
                    [ text "Fetch services" ]
                , renderServices model.services model.serviceId
                ]

        jobForm =
            form
                [ onSubmit SubmitJob
                , style
                    [ ( "max-width", "500px" )
                    , ( "margin", "0 auto" )
                    ]
                ]
                [ Html.App.map PagesSchemaMsg <| Pages.Schema.render model.jobForm
                , div [ style boxStyle ]
                    [ button [ Attrs.type' "submit" ] [ text "Create Job" ]
                    ]
                ]
    in
        div []
            [ services
            , jobForm
            ]

fetchServices : ClientSettings -> Cmd Msg
fetchServices cfg =
    Task.perform ResponseError FetchServicesSuccess <|
        ServiceDescriptorSvc.list cfg


fetchService : Id -> ClientSettings -> Cmd Msg
fetchService id cfg =
    Task.perform ResponseError FetchServiceSuccess <|
        ServiceDescriptorSvc.get id cfg


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

entityRowStyle : List ( String, String )
entityRowStyle =
    [ ( "padding", "5px" )
    , ( "background", "#eee" )
    , ( "margin-top", "5px" )
    , ( "cursor", "pointer" )
    , ( "font-family", "menlo, monospace" )
    ]


