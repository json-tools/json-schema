module Pages.Vault exposing (init, render, update, Msg, Model)

import Fragments.Form as FragForm
import HttpBuilder exposing (Error, Response)
import Html exposing (text, div, button)
import Html.Events exposing (onClick, onSubmit)
import Html.Attributes as Attrs exposing (style)
import Json.Encode as Encode exposing (encode, null)
import Json.Decode as Decode exposing (value, Value, (:=))
import Task
import Dict
import String
import Layout exposing (boxStyle)
import Types exposing (ClientSettings, RequestConfig, Config, ApiEndpointDefinition)
import Markdown
import JsonSchema as JS exposing (Schema)
import Util exposing (performRequest, buildHeaders)


type alias Model =
    { responses : Dict.Dict String (Response Value)
    , inputs : Dict.Dict String Value
    , schemas : Dict.Dict String Schema
    , error : String
    , services : List Service
    , endpoints : Endpoints
    }


type alias Endpoints =
    Dict.Dict String ApiEndpointDefinition

schema : String -> Schema
schema str =
    case JS.fromString str of
        Err e ->
            let
                a =
                    Debug.log ("Can not parse schema" ++ str) e
            in
                JS.empty

        Ok s ->
            s


init : Config -> Model
init conf =
    Model
        -- responses
        Dict.empty
        -- inputs
        Dict.empty
        -- schemas
        (extractInputSchemas conf.endpoints)
        -- error
        ""
        -- services
        []
        -- endpoints
        (Dict.fromList conf.endpoints)

extractInputSchemas : List (String, ApiEndpointDefinition) -> Dict.Dict String Schema
extractInputSchemas endpoints =
    endpoints
        |> List.map (\(name, x) -> (name, JS.fromValue x.request |> Result.withDefault JS.empty))
        |> Dict.fromList
        

type PostAction
    = NoOp
    | FillPan
    | FillFake
    | ListServices


type Msg
    = ResponseError String (Error Value)
    | ResponseSuccess String (Response Value)
    | PerformRequest String
    | UpdateData String Value
    | PerformPostAction String
    | SelectService String


req : String -> Endpoints -> Dict.Dict String Value -> Result String ApiEndpointDefinition
req reqType endpoints inputs =
    let
        data =
            Dict.get reqType inputs
    in
        case Dict.get reqType endpoints of
            Just endpoint ->
                Ok endpoint

            Nothing ->
                Err <| "Endpoint " ++ reqType ++ " not found"


getSchema : String -> Model -> Schema
getSchema t { schemas } =
    schemas
        |> Dict.get t
        |> Maybe.withDefault JS.empty


send : HttpBuilder.RequestBuilder -> Task.Task (Error Value) (Response Value)
send =
    HttpBuilder.send
        (HttpBuilder.jsonReader value)
        (HttpBuilder.jsonReader value)


type alias Service =
    { id : String
    , name : String
    , schema : Value
    }


update : Msg -> Model -> ClientSettings -> ( Model, Cmd Msg )
update msg model clientSettings =
    case msg of
        SelectService name ->
            let
                set destName destPath x =
                    model.inputs
                        |> Dict.update destName
                            (\v ->
                                v
                                    |> Maybe.withDefault null
                                    |> JS.setValue (getSchema destName model) destPath x
                                    |> Just
                            )

                id =
                    case findService name of
                        Just serv ->
                            Encode.string serv.id

                        Nothing ->
                            Encode.string ""

                target =
                    Dict.get "service/create-job" model.schemas
                        |> Maybe.withDefault JS.empty

                applyServiceSchema name =
                    case findService name of
                        Just serv ->
                            model.schemas
                                |> Dict.insert "service/create-job" (JS.registerProperty "input" (JS.fromValue serv.schema |> Result.withDefault JS.empty) target)

                        Nothing ->
                            model.schemas

                findService : String -> Maybe Service
                findService name =
                    model.services
                        |> List.foldl
                            (\serv res ->
                                case res of
                                    Nothing ->
                                        if serv.name == name then
                                            Just serv
                                        else
                                            Nothing

                                    Just res ->
                                        Just res
                            )
                            Nothing
            in
                { model | schemas = applyServiceSchema name, inputs = set "service/create-job" [ "serviceId" ] id } ! []

        ResponseError name e ->
            case e of
                HttpBuilder.BadResponse resp ->
                    { model | responses = model.responses |> Dict.insert name resp } ! []

                _ ->
                    { model | error = toString e } ! []

        PerformRequest name ->
            let
                request =
                    req name model.endpoints model.inputs

                body =
                    model.inputs
                        |> Dict.get name 
            in
                case request of
                    Err s ->
                        { model | error = s } ! []

                    Ok request ->
                        { model | error = "" }
                            ! [ request
                                    |> performRequest clientSettings body
                                    |> Task.perform (ResponseError name) (ResponseSuccess name)
                              ]

        ResponseSuccess name resp ->
            let
                updatedModel =
                    { model | responses = model.responses |> Dict.insert name resp }
            in
                update (PerformPostAction name) updatedModel clientSettings

        PerformPostAction name ->
            let
                decodeService =
                    Decode.object3 Service
                        ("id" := Decode.string)
                        ("name" := Decode.string)
                        ("schema" := Decode.value)

                get sourceName sourcePath =
                    model.responses
                        |> Dict.get sourceName
                        |> (\s ->
                                case s of
                                    Nothing ->
                                        Encode.string ""

                                    Just resp ->
                                        Decode.decodeValue (Decode.at sourcePath Decode.value) resp.data
                                            |> Result.withDefault (Encode.string "")
                           )

                getList : String -> List String -> List Service
                getList sourceName sourcePath =
                    model.responses
                        |> Dict.get sourceName
                        |> (\s ->
                                case s of
                                    Nothing ->
                                        []

                                    Just resp ->
                                        Decode.decodeValue (Decode.at sourcePath <| Decode.list decodeService) resp.data
                                            |> Result.withDefault []
                           )

                set destName destPath x =
                    model.inputs
                        |> Dict.update destName
                            (\v ->
                                v
                                    |> Maybe.withDefault null
                                    |> JS.setValue (getSchema destName model) destPath x
                                    |> Just
                            )
            in
                case name of
                    "vault/create-otp" ->
                        { model
                            | inputs =
                                get "vault/create-otp" [ "id" ]
                                    |> set "vault/create-pan" [ "otp" ]
                        }
                            ! []

                    "vault/create-pan" ->
                        { model
                            | inputs =
                                get "vault/create-pan" [ "id" ]
                                    |> set "vault/create-fake-pan" [ "panId" ]
                        }
                            ! []

                    "service/list-services" ->
                        { model
                            | services = getList "service/list-services" [ "data" ]
                        }
                            ! []

                    _ ->
                        model ! []

        UpdateData reqType v ->
            { model | inputs = Dict.insert reqType v model.inputs } ! []


codeStyle : Html.Attribute msg
codeStyle =
    style
        [ ( "background", "#444" )
        , ( "color", "#eee" )
        , ( "max-width", "300px" )
        , ( "overflow", "auto" )
        ]


render : Model -> ClientSettings -> Html.Html Msg
render model clientSettings =
    let
        formatRequest kind =
            let
                request =
                    req kind model.endpoints model.inputs
            in
                case request of
                    Ok r ->
                        div [] [ renderRequest r clientSettings ]

                    Err s ->
                        text s

        renderHeaders h =
            List.map (\( header, value ) -> header ++ ": " ++ value) h
                |> String.join "\n"

        getHostname url =
            url
                |> String.split "/"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""

        serviceUrl s =
            if s == "vault" then
                clientSettings.vault
            else
                clientSettings.service

        renderRequest r clientSettings =
            let

               headers =
                   buildHeaders r clientSettings
                       |> (::) ( "Host", r.service |> serviceUrl |> getHostname )
                       |> renderHeaders

                {- TODO: grab body from inputs
                   body =
                       case r.body of
                           Nothing ->
                               ""

                           Just val ->
                               encode 2 val
                -}
            in
                "```http\n"
                    ++ r.method
                    ++ " "
                    ++ r.pathname
                    ++ " HTTP/1.1\n"
                    ++ headers
                    ++
                        "\n\n"
                    -- ++ body
                    ++
                        "\n```"
                    |> Markdown.toHtml [ markdownCodeStyle ]

        markdownCodeStyle =
            style <|
                boxStyle
                    ++ [ ( "margin", "0 0 10px 0" )
                       , ( "background", "#333" )
                       , ( "color", "#ddd" )
                       , ( "font-size", "12px" )
                       , ( "line-height", "1.2em" )
                       , ( "border-color", "#928374" )
                       , ( "max-height", "400px" )
                       , ( "padding", "0 10px" )
                       ]

        renderJsonBody headers data =
            data
                |> encode 2
                |> (\s -> "```http\n" ++ (renderHeaders headers) ++ "\n\n" ++ s ++ "\n```")
                |> Markdown.toHtml [ markdownCodeStyle ]

        formatResponse requestType =
            case Dict.get requestType model.responses of
                Nothing ->
                    div []
                        [ div
                            [ style
                                (boxStyle
                                    ++ [ ( "margin", "0 0 10px 0" )
                                       , ( "border-color", "slategray" )
                                         -- , ( "background", "#222" )
                                       , ( "color", "slategray" )
                                       ]
                                )
                            ]
                            [ text "Request is not sent"
                            ]
                        ]

                Just resp ->
                    div []
                        [ div
                            [ style
                                (boxStyle
                                    ++ [ ( "margin", "0 0 10px 0" )
                                       , ( "padding", "0 10px" )
                                       , ( "border-color"
                                         , (if 200 <= resp.status && resp.status < 400 then
                                                "olivedrab"
                                            else
                                                "crimson"
                                           )
                                         )
                                       ]
                                )
                            ]
                            --[ text <| resp.url ++ " -- "
                            [ "```http\nHTTP/1.1 " ++ (toString resp.status) ++ " " ++ resp.statusText ++ "\n```" |> Markdown.toHtml [ style [ ( "padding", "0" ), ( "font-size", "12px" ) ] ]
                            ]
                        , resp.data |> renderJsonBody (resp.headers |> Dict.toList)
                        ]

        column bg fg width =
            div
                [ style
                    [ ( "padding", "10px" )
                    , ( "width", width )
                    , ( "flex-shrink", "0" )
                    , ( "background", bg )
                    , ( "color", fg )
                    , ( "box-sizing", "border-box" )
                    ]
                ]

        renderBlock label guide buttonText requestBuilder postAction childNodes =
            let
                name =
                    requestBuilder

                schema =
                    getSchema name model

                data =
                    model.inputs
                        |> Dict.get name
                        |> Maybe.withDefault null
            in
                div [ style [ ( "border-bottom", "1px solid #aaa" ) ] ]
                    [ div [ style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
                        [ column "rgba(249, 245, 236, 0.58)"
                            "#282828"
                            "60%"
                            [ Html.form [ onSubmit (PerformRequest requestBuilder) ]
                                [ Html.h3 [] [ text label ]
                                , Markdown.toHtml [] guide
                                , div [ style [ ( "max-height", "500px" ), ( "overflow", "auto" ) ] ]
                                    [ FragForm.render
                                        { validationErrors = Dict.empty
                                        , schema = schema
                                        , data = data
                                        , onInput = UpdateData requestBuilder
                                        }
                                    ]
                                , div []
                                    [ button
                                        [ Attrs.type' "submit"
                                        , style
                                            [ ( "font-family", "Iosevka, monospace" )
                                            , ( "font-size", "14px" )
                                            , ( "width", "100%" )
                                            , ( "height", "40px" )
                                            , ( "background", "white" )
                                            , ( "border-radius", "2px" )
                                            , ( "border", "1px solid #ddd" )
                                            , ( "font-weight", "bold" )
                                            ]
                                        ]
                                        [ text buttonText ]
                                    ]
                                , div [] childNodes
                                ]
                            ]
                        , column "#282828"
                            "cornsilk"
                            "40%"
                            [ Html.h3 [] [ text "Request" ]
                            , formatRequest requestBuilder
                            , Html.h3 [] [ text "Response" ]
                            , formatResponse requestBuilder
                            ]
                        ]
                    ]

        error =
            case model.error of
                "" ->
                    text ""

                _ ->
                    div [ style (boxStyle ++ [ ( "border-color", "red" ), ( "position", "fixed" ), ( "background", "white" ), ( "bottom", "10px" ), ( "right", "10px" ) ]) ] [ text model.error ]
    in
        div []
            [ error
            , renderBlock
                "1. Request OTP to authorize saving PAN"
                """
To save a payment card securely in our vault we issue one-time password (OTP). The resulting OTP can be used only once to save a single PAN.
                """
                "Create otp"
                "vault/create-otp"
                FillPan
                []
            , renderBlock
                "2. Save PAN"
                """
Next you store your user’s PAN in the vault. This endpoint is the only one not authenticated with your client secret key, it requires OTP in order to authorise the request.

The result of this call must be stored in your database as the permanent id of the user's PAN. It can not be used to retrieve or decrypt the card, it can only be used to issue a replacement token.
                """
                "Use otp -> create PAN"
                "vault/create-pan"
                FillFake
                []
            , renderBlock
                "3. Issue fake PAN given panId"
                """
This endpoint creates a token which will then be used to start a job which requires a PAN. The token expires after some time (currently 1 hour). A new token must be issued for each new job. The same token can't be used twice.
                """
                "Exchange panId -> fake PAN"
                "vault/create-fake-pan"
                NoOp
                []
            , renderBlock
                "4. Fetch list of services"
                """
The Automation cloud offers a number of automation services. Each of these services requires particular input data in JSON format. This endpoint provides list of the available services with schemas describing the format of the input data.
                 """
                "Show me what you can do"
                "service/list-services"
                ListServices
                [ renderServices model SelectService ]
            , renderBlock
                "5. Submit job"
                """
This is the starting point of the automation process, and creates your automation job. This is a function call with an object as an argument, and it returns the object which will represent your job (including the output, errors and yields).
                 """
                "Do your job"
                "service/create-job"
                NoOp
                []
            ]


renderServices : Model -> (String -> msg) -> Html.Html msg
renderServices model selected =
    let
        renderService s =
            Html.option [ Attrs.value s.name ] [ text s.name ]
    in
        if List.length model.services > 0 then
            div [ style boxStyle ]
                [ text "Select service: "
                , Html.select [ Html.Events.onInput selected ] <|
                    Html.option [ Attrs.value "" ] [ text "Select Service" ]
                        :: List.map renderService model.services
                ]
        else
            text ""
