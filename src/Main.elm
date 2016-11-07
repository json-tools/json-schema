port module Main exposing (..)

import Json.Decode as Decode exposing (Decoder, maybe, string, bool, succeed, (:=))
import Json.Decode.Extra as DecodeExtra exposing ((|:), withDefault, lazy)
import Json.Encode as Encode exposing (Value)
import Http
import HttpBuilder
import Html exposing (div, span, button, text, form, input, ul, li)
import Html.App exposing (program)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style)
import Task
import Dict
import Set
import String
import JsonSchema as JS exposing (Schema)


main : Program Never
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Id =
    String


type alias Model =
    { services : Maybe (List ServiceDescriptor)
    , error : String
    , validationErrors : ValidationErrors
    , credentials : String
    , schema : Maybe Schema
    , input : Maybe Value
    , serviceId : Id
    , job : Maybe Job
    }


type alias ServiceDescriptor =
    { id : Id
    , name : String
    , type' : String
    }


type alias ValidationErrors =
    Dict.Dict (List String) String


type alias Job =
    { id : Id
    , state : String
    }


fetchServices : String -> Cmd Msg
fetchServices credentials =
    Task.perform FetchError
        FetchServicesSuccess
        (Http.fromJson
            (Decode.at [ "data" ] (Decode.list decodeService))
            (Http.send
                Http.defaultSettings
                { verb = "GET"
                , headers = [ ( "Authorization", "Basic " ++ credentials ) ]
                , url = "http://localhost:3000/services"
                , body = Http.empty
                }
            )
        )


fetchSchema : String -> Id -> Cmd Msg
fetchSchema credentials id =
    Task.perform
        ResponseError
        FetchSchemaSuccess
        (HttpBuilder.get ("http://localhost:3000/services/" ++ id)
            |> HttpBuilder.withHeader "Authorization" ("Basic " ++ credentials)
            |> HttpBuilder.send
                (HttpBuilder.jsonReader (Decode.at [ "schema" ] Decode.value))
                (HttpBuilder.stringReader)
         -- (Decode.at [ "schema" ] JS.decodeSchema)
         -- (Http.send
         --     Http.defaultSettings
         --     { verb = "GET"
         --     , headers = [ ( "Authorization", "Basic " ++ credentials ) ]
         --     , url =
         --     , body = Http.empty
         --     }
         -- )
        )


submitJob : String -> Id -> Value -> Cmd Msg
submitJob credentials serviceId inputData =
    Task.perform
        SubmitJobError
        SubmitJobSuccess
        (HttpBuilder.post "http://localhost:3000/jobs"
            |> HttpBuilder.withHeader "Authorization" ("Basic " ++ credentials)
            |> HttpBuilder.withJsonBody
                (Encode.object
                    [ ( "service_id", Encode.string serviceId )
                    , ( "input", inputData )
                    ]
                )
            |> HttpBuilder.send
                (HttpBuilder.jsonReader decodeJob)
                (HttpBuilder.jsonReader (Decode.at [ "details", "input" ] (Decode.list Decode.string)))
        )


decodeJob : Decoder Job
decodeJob =
    succeed Job
        |: ("id" := string)
        |: ("state" := string)


decodeService : Decoder ServiceDescriptor
decodeService =
    succeed ServiceDescriptor
        |: ("id" := string)
        |: ("name" := string)
        |: ("type" := string)


init : ( Model, Cmd msg )
init =
    Model
        -- list services
        Nothing
        -- error
        ""
        -- validationErrors
        Dict.empty
        -- credentials
        "MGNkNTcxNjZjMGQ1YzE3NTMwZWE3NzZkOTExZDNlMDliMjAzNTUzNjMyNWFhMzVjOg=="
        -- schema
        Nothing
        -- input
        Nothing
        -- serviceId
        ""
        -- job
        Nothing
        ! []



-- UPDATE


type Msg
    = NoOp
    | SetCredentials String
    | FetchServices
    | FetchError Http.Error
    | FetchServicesSuccess (List ServiceDescriptor)
    | FetchSchema Id
    | FetchSchemaSuccess (HttpBuilder.Response Value)
    | UpdateProperty (List String) Value
    | SubmitJob
    | SubmitJobError (HttpBuilder.Error (List String))
    | SubmitJobSuccess (HttpBuilder.Response Job)
    | ResponseError (HttpBuilder.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        -- case msg of
        NoOp ->
            model ! []

        SetCredentials c ->
            { model | credentials = c } ! []

        FetchServices ->
            model ! [ fetchServices model.credentials ]

        FetchError err ->
            { model | error = toString err } ! []

        ResponseError err ->
            { model | error = toString err } ! []

        FetchServicesSuccess svcs ->
            { model | services = Just svcs } ! []

        FetchSchema id ->
            { model | serviceId = id } ! [ fetchSchema model.credentials id ]

        FetchSchemaSuccess { data } ->
            let
                updatedSchema =
                    Debug.log "updated schema" ((Result.withDefault JS.empty (JS.fromValue data)))
            in
                { model | schema = Just updatedSchema } ! []

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
                    model ! [ submitJob model.credentials model.serviceId input ]

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
                    in
                        { model
                            | validationErrors =
                                List.foldl (\str -> Dict.insert (key str) (val str))
                                    Dict.empty
                                    data
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
    , ( "font-family", "iosevka, firacode, menlo, monospace" )
    , ( "font-size", "14px" )
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
            div [ style boxStyle ]
                [ input
                    [ Attrs.value model.credentials
                    , Attrs.autocomplete False
                    , Attrs.placeholder "Client secret (go grab it from db)"
                    , Attrs.name "credentials"
                    , onInput SetCredentials
                    , style
                        [ ( "width", "50%" )
                        , ( "font-family", "menlo, monospace" )
                        , ( "font-size", "12px" )
                        ]
                    ]
                    []
                , button [ onClick FetchServices ] [ text "Fetch services" ]
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
                    form [ onSubmit SubmitJob ]
                        [ renderSchema schema [] (Maybe.withDefault (JS.defaultFor schema) model.input) schema model.validationErrors
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


renderSchema : Schema -> List String -> Value -> Schema -> ValidationErrors -> Html.Html Msg
renderSchema schema path inputData rootSchema validationErrors =
    let
        renderRow : ( String, Schema ) -> Html.Html Msg
        renderRow ( name, property ) =
            let
                required =
                    Set.member name schema.required

                propPath =
                    path ++ [ name ]

                validationError =
                    Dict.get propPath validationErrors
                        |> Maybe.withDefault ""

                rowStyle =
                    if String.isEmpty validationError then
                        boxStyle
                    else
                        boxStyle ++ [ ( "border-color", "red" ) ]
            in
                div [ style rowStyle ]
                    [ text
                        (if required then
                            "* "
                         else
                            ""
                        )
                    , text (name ++ ": ")
                    , renderProperty property required propPath inputData rootSchema validationErrors
                    , if String.isEmpty validationError then
                        text ""
                      else
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
                    ]

        renderProps (JS.Properties props) =
            List.map renderRow props
    in
        div [] (renderProps schema.properties)


renderProperty : Schema -> Bool -> List String -> Value -> Schema -> ValidationErrors -> Html.Html Msg
renderProperty prop required path inputData schema validationErrors =
    case prop.type_ of
        "string" ->
            renderInput prop required path inputData schema

        "integer" ->
            renderInput prop required path inputData schema

        "object" ->
            renderSchema prop path inputData schema validationErrors

        "array" ->
            case prop.items of
                Just (JS.ArrayItems itemDefinition) ->
                    renderArray itemDefinition required path inputData schema validationErrors

                Nothing ->
                    text "missing item definition for array"

        _ ->
            text ("Unknown property type: " ++ prop.type_)


renderArray : Schema -> Bool -> List String -> Value -> Schema -> ValidationErrors -> Html.Html Msg
renderArray p required path inputData s validationErrors =
    let
        schema =
            s

        property =
            p

        length =
            JS.getLength schema path inputData
    in
        div []
            (([0..(length - 1)]
                |> List.map
                    (\index ->
                        div [ style boxStyle ]
                            [ text ("#" ++ (toString (index + 1)))
                            , renderProperty
                                property
                                required
                                (path ++ [ toString index ])
                                inputData
                                schema
                                validationErrors
                            ]
                    )
             )
                ++ [ span
                        [ onClick (UpdateProperty (path ++ [ toString length ]) (JS.defaultFor property))
                        , style
                            [ ("background", "lightgoldenrodyellow")
                            , ("cursor", "pointer" )
                            , ("border", "1px solid royalblue")
                            , ("color", "royalblue")
                            , ("margin", "10px")
                            , ("padding", "5px")
                            , ("display", "inline-block")
                            ]
                        ]
                        [ text "Add item" ]
                   ]
            )


renderInput : Schema -> Bool -> List String -> Value -> Schema -> Html.Html Msg
renderInput property required path inputData schema =
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
    in
        input
            [ Attrs.required required
              -- , Attrs.name name
            , Attrs.title title
            , Attrs.pattern pattern
            , Attrs.type' inputType
            , onInput
                (\s ->
                    UpdateProperty path
                        (case property.type_ of
                            "integer" ->
                                Encode.int (Result.withDefault 0 (String.toInt s))

                            _ ->
                                Encode.string s
                        )
                )
            , style [ ( "font-family", "menlo, monospace" ), ( "width", "100%" ) ]
            , Attrs.value
                (case property.type_ of
                    "integer" ->
                        JS.getInt schema path inputData
                            |> toString

                    _ ->
                        JS.getString schema path inputData
                )
            ]
            []


encodeDict : Dict.Dict String Value -> Value
encodeDict dict =
    Encode.object (Dict.toList dict)


decodeDict : Value -> Dict.Dict String Value
decodeDict val =
    Decode.decodeValue (Decode.dict Decode.value) val
        |> Result.withDefault Dict.empty


decodeList : Value -> List Value
decodeList val =
    Decode.decodeValue (Decode.list Decode.value) val
        |> Result.withDefault []


renderServices : List ServiceDescriptor -> Id -> Html.Html Msg
renderServices services id =
    div []
        [ div [ style boxStyle ]
            [ div []
                (services
                    |> List.map
                        (\svc ->
                            span
                                [ style
                                    (entityRowStyle
                                        ++ [ ( "margin-right", "10px" )
                                           , ( "display", "inline-block" )
                                           , ( "font-weight", "bold" )
                                           , ( "background"
                                             , if id == svc.id then
                                                "black"
                                               else
                                                "lightgrey"
                                             )
                                           , ( "color"
                                             , if id == svc.id then
                                                "lightyellow"
                                               else
                                                "black"
                                             )
                                           ]
                                    )
                                , onClick (FetchSchema svc.id)
                                ]
                                [ text (svc.name) ]
                        )
                )
            ]
        ]
