port module Main exposing (..)

import Json.Decode as Decode exposing (Decoder, maybe, string, bool, succeed, (:=))
import Json.Decode.Extra as DecodeExtra exposing ((|:), withDefault, lazy)
import Json.Encode as Encode exposing (Value)
import Http
import Html exposing (div, button, text, form, input, ul, li)
import Html.App exposing (program)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style)
import Task
import Dict
import Set
import String


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
    , credentials : String
    , schema : Maybe Schema
    , input : InputData
    , serviceId : Id
    , job : Maybe Job
    }


type alias InputData =
    Dict.Dict String Value


type alias ServiceDescriptor =
    { id : Id
    , name : String
    , type' : String
    }


type alias Schema =
    { properties : Properties
    , required : Set.Set String
    , type' : Maybe String
    , format : Maybe String
    , ref : Maybe String
    , definitions : Properties
    }


type Properties
    = Properties (List ( String, Schema ))


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
    Task.perform FetchError
        FetchSchemaSuccess
        (Http.fromJson
            (Decode.at [ "schema" ] decodeSchema)
            (Http.send
                Http.defaultSettings
                { verb = "GET"
                , headers = [ ( "Authorization", "Basic " ++ credentials ) ]
                , url = "http://localhost:3000/services/" ++ id
                , body = Http.empty
                }
            )
        )


submitJob : String -> Id -> InputData -> Cmd Msg
submitJob credentials serviceId inputData =
    Task.perform FetchError
        FetchJobSuccess
        (Http.fromJson
            decodeJob
            (Http.send
                Http.defaultSettings
                { verb = "POST"
                , headers = [ ( "Authorization", "Basic " ++ credentials ) ]
                , url = "http://localhost:3000/jobs"
                , body =
                    Http.string
                        (Encode.encode 0
                            (Encode.object
                                [ ( "service_id", Encode.string serviceId )
                                , ( "input", Encode.object (Dict.toList inputData) )
                                ]
                            )
                        )
                }
            )
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


decodeSchema : Decoder Schema
decodeSchema =
    succeed Schema
        |: (withDefault (Properties []) ("properties" := decodeProperties))
        |: (withDefault Set.empty ("required" := DecodeExtra.set string))
        |: (maybe ("type" := string))
        |: (maybe ("format" := string))
        |: (maybe ("$ref" := string))
        |: (withDefault (Properties []) ("definitions" := decodeProperties))



--     Decode.object4 (\p r t f -> Schema { properties = p, required = r, type' = t, format = f })
--         ("properties" := decodeProperties)
--         (withDefault Set.empty ("required" := DecodeExtra.set string))
--         ("type" := string)
--         (maybe ("format" := string))


decodeProperties : Decoder Properties
decodeProperties =
    succeed Properties
        |: Decode.keyValuePairs (DecodeExtra.lazy (\_ -> decodeSchema))


init : ( Model, Cmd msg )
init =
    Model
        -- list services
        Nothing
        -- error
        ""
        -- credentials
        "OGZlNmVjOWIzMDZhOTY0YjZmOTRhZTFlODkwYTQ3YzQzY2MzMWZhZWRlYTA1Y2FmOg=="
        -- schema
        Nothing
        -- input
        Dict.empty
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
    | FetchSchemaSuccess Schema
    | UpdateProperty (List String) String
    | SubmitJob
    | FetchJobSuccess Job


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- case Debug.log "update" msg of
    case msg of
        NoOp ->
            model ! []

        SetCredentials c ->
            { model | credentials = c } ! []

        FetchServices ->
            model ! [ fetchServices model.credentials ]

        FetchError err ->
            { model | error = toString err } ! []

        FetchServicesSuccess svcs ->
            { model | services = Just svcs } ! []

        FetchSchema id ->
            { model | serviceId = id } ! [ fetchSchema model.credentials id ]

        FetchSchemaSuccess schema ->
            { model | schema = Just schema } ! []

        UpdateProperty path value ->
            let
                upd =
                    update value path model.input

                encodeDict dict =
                    Encode.object (Dict.toList dict)

                valueDecoder =
                    Decode.oneOf
                        [ Decode.map (\s -> Encode.string s) string
                        , Decode.map (\d -> encodeDict d) (Decode.dict valueDecoder)
                        ]

                decodeDict val =
                    Decode.decodeValue (Decode.dict valueDecoder) val
                        |> Result.withDefault Dict.empty

                update finalValue subPath dataNode =
                    case subPath of
                        name :: [] ->
                            (Dict.insert name
                                (Encode.string finalValue)
                                dataNode
                            )

                        head :: tail ->
                            (Dict.get head dataNode
                                |> Maybe.withDefault (encodeDict Dict.empty)
                                |> decodeDict
                                |> update finalValue tail
                                |> encodeDict
                                |> Dict.insert head
                            )
                                dataNode
                                |> Debug.log (head ++ ":tail")

                        _ ->
                            dataNode
            in
                { model | input = upd } ! []

        SubmitJob ->
            model ! [ submitJob model.credentials model.serviceId model.input ]

        FetchJobSuccess job ->
            { model | job = Just job } ! []



-- port alert : String -> Cmd msg
-- port log : (String -> msg) -> Sub msg
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
                    ]
                    []
                , button [ onClick FetchServices ] [ text "Fetch services" ]
                ]

        services =
            case model.services of
                Nothing ->
                    text ""

                Just svcs ->
                    renderServices svcs

        schema =
            case model.schema of
                Nothing ->
                    text ""

                Just schema ->
                    form [ onSubmit SubmitJob ]
                        [ renderSchema schema [] model.input schema
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


traverse : Schema -> List String -> Maybe Schema
traverse schema path =
    let
        getDefinition (Properties defs) name =
            List.foldl
                (\( n, prop ) result ->
                    if name == n then
                        Just prop
                    else
                        result
                )
                Nothing
                defs
    in
        case path of
            section :: name :: [] ->
                case section of
                    "definitions" ->
                        getDefinition schema.definitions name

                    _ ->
                        Nothing

            _ ->
                Nothing


renderSchema : Schema -> List String -> InputData -> Schema -> Html.Html Msg
renderSchema schema path inputData rootSchema =
    let
        renderRow : ( String, Schema ) -> Html.Html Msg
        renderRow ( name, property ) =
            let
                digDefinition ref =
                    String.split "/" ref
                        |> List.drop 1
                        |> traverse rootSchema

                required =
                    Set.member name schema.required

                expandedProperty =
                    case property.ref of
                        Nothing ->
                            Just property

                        Just ref ->
                            digDefinition ref
            in
                div [ style boxStyle ]
                    [ text (name ++ ": ")
                    , renderProperty expandedProperty (path ++ [ name ]) inputData schema
                    , text
                        (if required then
                            " *"
                         else
                            ""
                        )
                    ]

        renderProps (Properties props) =
            List.map renderRow props
    in
        div [] (renderProps schema.properties)


renderProperty : Maybe Schema -> List String -> InputData -> Schema -> Html.Html Msg
renderProperty property path inputData schema =
    case property of
        Nothing ->
            text ("Missing property definition: " ++ (String.join "/" path))

        Just prop ->
            case prop.type' of
                Just "string" ->
                    renderInput prop path inputData schema

                Just "integer" ->
                    renderInput prop path inputData schema

                Just "object" ->
                    renderSchema prop path inputData schema

                _ ->
                    text ("Unknown property type: " ++ (Maybe.withDefault "null" prop.type'))


renderInput : Schema -> List String -> InputData -> Schema -> Html.Html Msg
renderInput property path inputData schema =
    let
        isRequired =
            Set.member (Maybe.withDefault "" (List.head path)) schema.required

        inputType =
            case property.format of
                Just "uri" ->
                    "url"

                _ ->
                    case property.type' of
                        Just "integer" ->
                            "number"

                        _ ->
                            "text"

        pattern =
            case property.format of
                Just "uuid" ->
                    "[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}"

                _ ->
                    ".*"

        title =
            case property.format of
                Just "uuid" ->
                    "Enter UUID like: 6a6eb029-06d9-4d4f-b257-ada7233b6086"

                Just "uri" ->
                    "Enter URL"

                _ ->
                    ""
    in
        input
            [ Attrs.required isRequired
              -- , Attrs.name name
            , Attrs.title title
            , Attrs.pattern pattern
            , Attrs.type' inputType
            , onInput (UpdateProperty path)
            , style [ ( "font-family", "menlo, monospace" ), ( "width", "100%" ) ]
            , Attrs.value (getString inputData path property)
            ]
            []


getString : InputData -> List String -> Schema -> String
getString inputData path schema =
    getValue path inputData
        |> Decode.decodeValue string
        |> Result.withDefault ""


getValue : List String -> InputData -> Value
getValue path inputData =
    let
        valueDecoder =
            Decode.oneOf
                [ Decode.map (\s -> Encode.string s) string
                , Decode.map (\d -> encodeDict d) (Decode.dict valueDecoder)
                ]

        decodeDict val =
            Decode.decodeValue (Decode.dict valueDecoder) val
                |> Result.withDefault Dict.empty

        encodeDict dict =
            Encode.object (Dict.toList dict)
    in
        case path of
            head :: [] ->
                inputData
                    |> Dict.get head
                    |> Maybe.withDefault (Encode.string "")

            head :: tail ->
                inputData
                    |> Dict.get head
                    |> Maybe.withDefault (Encode.object [])
                    |> decodeDict
                    |> getValue tail

            [] ->
                Encode.string ""


renderServices : List ServiceDescriptor -> Html.Html Msg
renderServices services =
    div []
        [ div [ style boxStyle ]
            [ div []
                (services
                    |> List.map
                        (\svc ->
                            div
                                [ style entityRowStyle
                                , onClick (FetchSchema svc.id)
                                ]
                                [ text ("Service " ++ svc.id ++ ": " ++ svc.name) ]
                        )
                )
            ]
        ]
