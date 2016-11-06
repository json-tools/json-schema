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


type Schema
    = Schema SchemaDefinition


type alias SchemaDefinition =
    { type' : String
    , required : Set.Set String
    , format : Maybe String
    , ref : Maybe String
    , enum : Maybe (List String)
    , items : Maybe Schema
    , properties : List ( String, Schema )
    , definitions : List ( String, Schema )
    }


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


decodeSchema : Decoder Schema
decodeSchema =
    Decode.map Schema
        (succeed SchemaDefinition
            |: (withDefault "object" ("type" := string))
            |: (withDefault Set.empty ("required" := DecodeExtra.set string))
            |: (maybe ("format" := string))
            |: (maybe ("$ref" := string))
            |: (maybe ("enum" := (Decode.list string)))
            |: (maybe ("items" := (DecodeExtra.lazy (\_ -> decodeSchema))))
            |: (withDefault [] ("properties" := (DecodeExtra.lazy (\_ -> Decode.keyValuePairs decodeSchema))))
            |: (withDefault [] ("definitions" := (DecodeExtra.lazy (\_ -> Decode.keyValuePairs decodeSchema))))
        )


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
    | FetchSchemaSuccess Schema
    | UpdateProperty (List String) Value
    | SubmitJob
    | SubmitJobError (HttpBuilder.Error (List String))
    | SubmitJobSuccess (HttpBuilder.Response Job)


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

        FetchServicesSuccess svcs ->
            { model | services = Just svcs } ! []

        FetchSchema id ->
            { model | serviceId = id } ! [ fetchSchema model.credentials id ]

        FetchSchemaSuccess schema ->
            let
                updatedSchema =
                    Debug.log "updated schema" (expandRefs schema)
            in
                { model | schema = Just updatedSchema } ! []

        UpdateProperty path val ->
            let
                upd path schema value =
                    case model.schema of
                        Just schema ->
                            Just
                                (updateValue
                                    value
                                    path
                                    schema
                                    (Maybe.withDefault (defaultFor schema) model.input)
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
                        [ renderSchema schema [] (Maybe.withDefault (defaultFor schema) model.input) schema model.validationErrors
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
traverse (Schema schema) path =
    case path of
        section :: name :: [] ->
            case section of
                "definitions" ->
                    getDefinition schema.definitions name

                _ ->
                    Nothing

        _ ->
            Nothing


expandRefs : Schema -> Schema
expandRefs (Schema rootSchema) =
    let
        digDefinition : String -> Schema -> Schema
        digDefinition ref (Schema node) =
            String.split "/" ref
                |> List.drop 1
                |> Debug.log ("expandRef " ++ ref ++ " will traverse")
                |> traverse (Schema rootSchema)
                |> Maybe.withDefault (Schema node)

        walk : Schema -> Schema
        walk (Schema node) =
            let
                n =
                    (Schema node)
            in
                case node.ref of
                    Just ref ->
                        digDefinition ref n

                    Nothing ->
                        Schema
                            { node
                                | properties = List.map (\( key, def ) -> ( key, walk def )) node.properties
                            }
    in
        walk (Schema rootSchema)


renderSchema : Schema -> List String -> Value -> Schema -> ValidationErrors -> Html.Html Msg
renderSchema (Schema schema) path inputData (Schema rootSchema) validationErrors =
    let
        renderRow : ( String, Schema ) -> Html.Html Msg
        renderRow ( name, Schema property ) =
            let
                required =
                    Set.member name schema.required

                propPath =
                    path ++ [ name ]
            in
                div [ style boxStyle ]
                    [ text
                        (if required then
                            "* "
                         else
                            ""
                        )
                    , text (name ++ ": ")
                    , renderProperty (Schema property) required propPath inputData (Schema rootSchema) validationErrors
                    , text (Maybe.withDefault "" (Dict.get propPath validationErrors))
                    ]

        renderProps props =
            List.map renderRow props
    in
        div [] (renderProps schema.properties)


renderProperty : Schema -> Bool -> List String -> Value -> Schema -> ValidationErrors -> Html.Html Msg
renderProperty (Schema prop) required path inputData (Schema schema) validationErrors =
    case prop.type' of
        "string" ->
            renderInput (Schema prop) required path inputData (Schema schema)

        "integer" ->
            renderInput (Schema prop) required path inputData (Schema schema)

        "object" ->
            renderSchema (Schema prop) path inputData (Schema schema) validationErrors

        "array" ->
            case prop.items of
                Just itemDefinition ->
                    renderArray itemDefinition required path inputData (Schema schema) validationErrors

                Nothing ->
                    text "missing item definition for array"

        _ ->
            text ("Unknown property type: " ++ prop.type')


renderArray : Schema -> Bool -> List String -> Value -> Schema -> ValidationErrors -> Html.Html Msg
renderArray (Schema p) required path inputData (Schema s) validationErrors =
    let
        schema =
            (Schema s)

        property =
            (Schema p)

        length =
            getLength inputData path property schema
    in
        div []
            (([0..(length - 1)]
                |> List.map
                    (\index ->
                        renderProperty
                            property
                            required
                            (path ++ [ toString index ])
                            inputData
                            schema
                            validationErrors
                    )
             )
                ++ [ span
                        [ onClick (UpdateProperty (path ++ [ toString length ]) (defaultFor property))
                        ]
                        [ text "Add item" ]
                   ]
            )


renderInput : Schema -> Bool -> List String -> Value -> Schema -> Html.Html Msg
renderInput (Schema property) required path inputData (Schema schema) =
    let
        inputType =
            case property.format of
                Just "uri" ->
                    "url"

                _ ->
                    case property.type' of
                        "integer" ->
                            "number"

                        _ ->
                            "text"

        pattern =
            case property.format of
                Just "uuid" ->
                    "[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}"

                Just "date" ->
                    "\\d{4}-[01]\\d-[0123]\\d"

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
                        (case property.type' of
                            "integer" ->
                                Encode.int (Result.withDefault 0 (String.toInt s))

                            _ ->
                                Encode.string s
                        )
                )
            , style [ ( "font-family", "menlo, monospace" ), ( "width", "100%" ) ]
            , Attrs.value (getString path (Schema schema) inputData)
            ]
            []


getString : List String -> Schema -> Value -> String
getString path schema inputData =
    getValue path schema inputData
        |> Decode.decodeValue string
        |> Result.withDefault "undefined"


getLength : Value -> List String -> Schema -> Schema -> Int
getLength inputData path property rootSchema =
    getValue path rootSchema inputData
        |> decodeList
        |> List.length


defaultFor : Schema -> Value
defaultFor (Schema schema) =
    case schema.type' of
        "object" ->
            Encode.object []

        "array" ->
            Encode.list []

        _ ->
            Encode.string ""


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


getDefinition : List ( String, Schema ) -> String -> Maybe Schema
getDefinition defs name =
    List.foldl
        (\( n, prop ) result ->
            if name == n then
                Just prop
            else
                result
        )
        Nothing
        defs


getValue : List String -> Schema -> Value -> Value
getValue path (Schema schema) inputData =
    (case path of
        [] ->
            inputData

        key :: tail ->
            case schema.type' of
                "object" ->
                    -- TODO: lookup definition using "ref" (need root schema for that)
                    case getDefinition schema.properties key of
                        Just def ->
                            inputData
                                |> decodeDict
                                |> Dict.get key
                                |> Maybe.withDefault (defaultFor def)
                                |> getValue tail def

                        Nothing ->
                            defaultFor (Schema schema)

                "array" ->
                    case schema.items of
                        Just def ->
                            inputData
                                |> decodeList
                                |> List.drop (String.toInt key |> (Result.withDefault 0))
                                |> List.head
                                |> Maybe.withDefault (Encode.string "")
                                |> getValue tail def

                        Nothing ->
                            defaultFor (Schema schema)

                _ ->
                    inputData
    )


withDefaultFor : Schema -> Maybe Value -> Value
withDefaultFor schema a =
    (Maybe.withDefault (defaultFor schema) a)


updateValue : Value -> List String -> Schema -> Value -> Value
updateValue finalValue subPath (Schema schema) dataNode =
    case subPath of
        [] ->
            finalValue

        key :: tail ->
            let
                subSchema =
                    (getDefinition schema.properties key)
            in
                case schema.type' of
                    "object" ->
                        let
                            node =
                                (decodeDict dataNode)

                            value =
                                withDefaultFor (Schema schema) (Dict.get key node)
                        in
                            Dict.insert key
                                (case subSchema of
                                    Just prop ->
                                        updateValue
                                            finalValue
                                            tail
                                            prop
                                            value

                                    Nothing ->
                                        finalValue
                                )
                                node
                                |> encodeDict

                    "array" ->
                        let
                            i =
                                Debug.log "array index is"
                                    (String.toInt key |> Result.withDefault 0)

                            list =
                                decodeList dataNode

                            len =
                                List.length list

                            update list =
                                let
                                    ( _, updated ) =
                                        List.foldl
                                            (\item ( index, list ) ->
                                                ( index + 1
                                                , list
                                                    ++ [ if index == i then
                                                            case schema.items of
                                                                Just prop ->
                                                                    updateValue
                                                                        finalValue
                                                                        tail
                                                                        prop
                                                                        item

                                                                Nothing ->
                                                                    finalValue
                                                         else
                                                            item
                                                       ]
                                                )
                                            )
                                            ( 0, [] )
                                            list
                                in
                                    updated
                        in
                            (if len > i then
                                Debug.log "just upd" list
                             else
                                case schema.items of
                                    Just def ->
                                        (list ++ [ defaultFor def ])

                                    Nothing ->
                                        list
                            )
                                |> update
                                |> Encode.list

                    _ ->
                        Debug.log "final value" finalValue


getPropertyType : String -> Schema -> Maybe String
getPropertyType name (Schema schema) =
    case getDefinition schema.properties name of
        Nothing ->
            Nothing

        Just (Schema prop) ->
            Just prop.type'


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
                                                "seashell"
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
