port module Main exposing (..)

import Models exposing (..)
import Types exposing (..)
import Json.Encode as Encode
import HttpBuilder
import Html exposing (div, span, button, text, form, input, ul, li)
import Html.App exposing (programWithFlags)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style)
import Task
import Dict
import Set
import String
import JsonSchema as JS
import Services.Job as JobSvc
import Services.ServiceDescriptor as ServiceDescriptorSvc
import Messages exposing (Msg, Msg(..))


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


type alias Path =
    List String


type alias Context =
    { root : Schema
    , data : Value
    , errors : ValidationErrors
    }


type alias ValidationErrors =
    Dict.Dict (List String) String


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

        SetClientSecretKey c ->
            let
                updateConfig config =
                    { config | clientSecretKey = c }

                updated =
                    updateConfig model.apiConfig

            in
                { model | apiConfig = updated } ! [ storeConfig (PersistedData updated)
 ]

        SetApiHost c ->
            let
                updateConfig config =
                    { config | apiHost = c }

                updated =
                    updateConfig model.apiConfig

            in
                { model | apiConfig = updated } ! [ storeConfig (PersistedData updated) ]

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

        UpdateProperty path val ->
            let
                upd path schema value =
                    case model.schema of
                        Just schema ->
                            Just
                                (JS.setValue
                                    schema
                                    path
                                    value
                                    (Maybe.withDefault (JS.defaultFor schema) model.input)
                                )

                        Nothing ->
                            Nothing
            in
                { model | input = upd path model.schema val } ! []

        SubmitJob ->
            case model.input of
                Just input ->
                    { model | error = "" }
                        ! [ Task.perform SubmitJobError SubmitJobSuccess <|
                                JobSvc.create model.apiConfig model.serviceId input
                          ]

                Nothing ->
                    model ! []

        SubmitJobSuccess { data } ->
            { model | job = Just data } ! []

        SubmitJobError err ->
            case err of
                HttpBuilder.BadResponse { data } ->
                    let
                        key str =
                            String.words str
                                |> List.head
                                |> Maybe.withDefault "."
                                |> String.dropLeft 1
                                |> String.split "."

                        val str =
                            String.words str
                                |> List.tail
                                |> Maybe.withDefault []
                                |> String.join " "

                        add str =
                            Dict.insert (key str) (val str)
                    in
                        { model
                            | validationErrors =
                                List.foldl add Dict.empty data
                        }
                            ! []

                _ ->
                    { model | error = toString err } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.none
        ]



-- VIEW


boxStyle : List ( String, String )
boxStyle =
    [ ( "border", "1px solid #ddd" )
    , ( "border-radius", "2px" )
    , ( "padding", "10px" )
    , ( "margin", "10px" )
    , ( "font-family", "iosevka, fira code, menlo, monospace" )
    , ( "font-size", "14px" )
    , ( "background-color", "rgba(0, 0, 0, 0.02)" )
    ]


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
            form [ style boxStyle, onSubmit FetchServices ]
                [ input
                    [ Attrs.value model.apiConfig.apiHost
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
                    [ Attrs.value model.apiConfig.clientSecretKey
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
                        (String.isEmpty model.apiConfig.clientSecretKey)
                    ]
                    [ text "Fetch services" ]
                ]

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
                        form [ onSubmit SubmitJob, style [ ( "max-width", "500px" ), ( "margin", "0 auto" ) ] ]
                            [ renderSchema context [] schema
                            , div [ style boxStyle ]
                                [ button [ Attrs.type' "submit" ] [ text "Create Job" ]
                                ]
                            ]

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


renderSchema : Context -> Path -> Schema -> Html.Html Msg
renderSchema context path node =
    let
        renderRow : ( String, Schema ) -> Html.Html Msg
        renderRow ( name, property ) =
            let
                required =
                    Set.member name node.required

                newPath =
                    path ++ [ name ]

                validationError =
                    Dict.get newPath context.errors
                        |> Maybe.withDefault ""

                hasError =
                    Dict.member newPath context.errors

                rowStyle =
                    if hasError then
                        boxStyle ++ [ ( "border-color", "red" ) ]
                    else
                        boxStyle
            in
                div [ style rowStyle ]
                    [ text
                        (if required then
                            "* "
                         else
                            ""
                        )
                    , text (name ++ ": ")
                    , renderProperty context property required newPath
                    , if hasError then
                        span
                            [ style
                                [ ( "display", "inline-block" )
                                , ( "font-style", "italic" )
                                , ( "background", "lightyellow" )
                                , ( "color", "red" )
                                  -- , ( "font-weight", "bold" )
                                , ( "margin-top", "5px" )
                                ]
                            ]
                            [ text validationError ]
                      else
                        text ""
                    ]
    in
        div [] <| JS.mapProperties node.properties renderRow


renderSelect : Context -> List String -> Schema -> Bool -> Path -> Html.Html Msg
renderSelect context options prop required path =
    options
        |> List.map (\opt -> Html.option [] [ text opt ])
        |> Html.select
            [ Html.Events.onInput (\s -> UpdateProperty path <| Encode.string s)
            , Attrs.value <| JS.getString context.root path context.data
            ]


renderProperty : Context -> Schema -> Bool -> Path -> Html.Html Msg
renderProperty context prop required path =
    case prop.type_ of
        "string" ->
            case prop.enum of
                Nothing ->
                    renderInput context prop required path

                Just enum ->
                    renderSelect context enum prop required path

        "integer" ->
            renderInput context prop required path

        "object" ->
            renderSchema context path prop

        "array" ->
            case prop.items of
                Just (JS.ArrayItemDefinition itemDefinition) ->
                    renderArray context itemDefinition required path

                Nothing ->
                    text "missing item definition for array"

        _ ->
            text ("Unknown property type: " ++ prop.type_)


renderArray : Context -> Schema -> Bool -> List String -> Html.Html Msg
renderArray context property required path =
    let
        length =
            JS.getLength context.root path context.data

        buttonStyle =
            [ ( "background", "white" )
            , ( "cursor", "pointer" )
            , ( "border", "1px solid ActiveBorder" )
            , ( "color", "ActiveBorder" )
            , ( "margin", "10px" )
            , ( "padding", "5px" )
            , ( "display", "inline-block" )
            ]

        renderItem index =
            div [ style boxStyle ]
                [ text ("#" ++ index)
                , renderProperty
                    context
                    property
                    required
                    (path ++ [ index ])
                ]
    in
        div []
            [ div [] <|
                List.map renderItem <|
                    List.map toString [0..(length - 1)]
            , span
                [ onClick (UpdateProperty (path ++ [ toString length ]) (JS.defaultFor property))
                , style buttonStyle
                ]
                [ text "Add item" ]
            ]


renderInput : Context -> Schema -> Bool -> Path -> Html.Html Msg
renderInput context property required path =
    let
        inputType =
            case property.format of
                Just "uri" ->
                    "url"

                Just "email" ->
                    "email"

                Just "date" ->
                    "date"

                Just "phone" ->
                    "tel"

                Just "color" ->
                    "color"

                _ ->
                    case property.type_ of
                        "integer" ->
                            "number"

                        _ ->
                            "text"

        pattern =
            case property.format of
                Just "uuid" ->
                    "[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}"

                Just "date" ->
                    "\\d{4}-[01]\\d-[0-3]\\d"

                _ ->
                    ".*"

        title =
            case property.format of
                Just "uuid" ->
                    "Enter UUID like: 6a6eb029-06d9-4d4f-b257-ada7233b6086"

                Just "date" ->
                    "Date format is YYYY-MM-DD"

                Just "uri" ->
                    "Enter URL"

                _ ->
                    ""

        update s =
            UpdateProperty path
                (case property.type_ of
                    "integer" ->
                        Encode.int (Result.withDefault 0 (String.toInt s))

                    _ ->
                        Encode.string s
                )
    in
        input
            [ Attrs.required required
              -- , Attrs.name name
            , Attrs.title title
            , Attrs.pattern pattern
            , Attrs.type' inputType
            , onInput update
            , style
                [ ( "font-family", "iosevka, menlo, monospace" )
                , ( "min-width", "90%" )
                , ( "font-size", "12px" )
                , ( "padding", "3px" )
                ]
            , Attrs.value <|
                if property.type_ == "integer" then
                    JS.getInt context.root path context.data |> toString
                else
                    JS.getString context.root path context.data
            ]
            []


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
