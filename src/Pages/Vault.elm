module Pages.Vault exposing (init, render, update, Msg, Model)

import Fragments.Form as FragForm
import Http exposing (Error, Response)
import Html exposing (text, div, button)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style)
import Json.Encode as Encode exposing (encode, null)
import Json.Decode as Decode exposing (value, Value, field)
import Regex
import Dict
import Set
import String
import Layout exposing (boxStyle)
import Types exposing (ClientSettings, RequestConfig, Config, ApiEndpointDefinition)
import Markdown
import JsonSchema as JS exposing (Schema, ArrayItemDefinition)
import Util exposing (performRequest, buildHeaders)


type alias Model =
    { responses : Dict.Dict String (Response String)
    , inputs : Dict.Dict String Value
    , schemas : Dict.Dict String Schema
    , outputSchemas : Dict.Dict String Schema
    , error : String
    , services : List Service
    , endpoints : Endpoints
    , dependencies : Dependencies
    , editAsJson : Set.Set String
    }


type alias Endpoints =
    Dict.Dict String ApiEndpointDefinition


init : Config -> Model
init conf =
    Model
        -- responses
        Dict.empty
        -- inputs
        Dict.empty
        -- schemas
        (extractInputSchemas conf.endpoints)
        -- outputSchemas
        (extractOutputSchemas conf.endpoints)
        -- error
        ""
        -- services
        []
        -- endpoints
        (Dict.fromList conf.endpoints)
        -- dependencies
        (extractDependencies conf.dependencies)
        -- editAsJson
        Set.empty


extractInputSchemas : List ( String, ApiEndpointDefinition ) -> Dict.Dict String Schema
extractInputSchemas endpoints =
    endpoints
        |> List.map (\( name, x ) -> ( name, JS.fromValue x.request |> Result.withDefault JS.empty ))
        |> Dict.fromList


extractOutputSchemas : List ( String, ApiEndpointDefinition ) -> Dict.Dict String Schema
extractOutputSchemas endpoints =
    endpoints
        |> List.map (\( name, x ) -> ( name, JS.fromValue x.response |> Result.withDefault JS.empty ))
        |> Dict.fromList


extractDependencies : List ( String, String ) -> Dependencies
extractDependencies source =
    let
        parseAddress str =
            case String.split "/" str of
                head :: tail ->
                    ( head, tail )

                _ ->
                    ( "", [] )
    in
        List.map
            (\( a, b ) -> ( parseAddress a, parseAddress b ))
            source


type alias JsonAddress =
    ( String, List String )


type alias Dependencies =
    List ( JsonAddress, JsonAddress )


type Msg
    = NoOp
    | HttpResponse String (Response String)
    | HttpError String Error
    | PerformRequest String
    | UpdateData String Value
    | UpdateJson String String
    | PerformPostAction String
    | SelectService String
    | EditAsJson String Bool


findEndpoint : String -> Endpoints -> Result String ApiEndpointDefinition
findEndpoint label endpoints =
    endpoints
        |> Dict.get label
        |> Result.fromMaybe ("Endpoint " ++ label ++ " not found")


getSchema : String -> Model -> Schema
getSchema t { schemas } =
    schemas
        |> Dict.get t
        |> Maybe.withDefault JS.empty


getOutputSchema : String -> Model -> Schema
getOutputSchema t { outputSchemas } =
    outputSchemas
        |> Dict.get t
        |> Maybe.withDefault JS.empty


type alias Service =
    { id : String
    , name : String
    , schema : Value
    }


update : Msg -> Model -> ClientSettings -> ( Model, Cmd Msg )
update msg model clientSettings =
    case msg of
        NoOp ->
            model ! []

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
                    getSchema "service/create-job" model

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

        HttpResponse name r ->
            let
                updatedModel =
                    { model | responses = model.responses |> Dict.insert name r }
            in
                update (PerformPostAction name) updatedModel clientSettings

        HttpError name err ->
            case err of
                Http.BadStatus resp ->
                    { model | responses = model.responses |> Dict.insert name resp } ! []

                _ ->
                    { model | error = toString err } ! []

        PerformRequest name ->
            let
                request =
                    findEndpoint name model.endpoints

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
                                    |> Http.send
                                        (\res ->
                                            case res of
                                                Ok r ->
                                                    HttpResponse name r

                                                Err err ->
                                                    HttpError name err
                                        )
                              ]

        PerformPostAction name ->
            let
                decodeService =
                    Decode.map3 Service
                        (field "id" Decode.string)
                        (field "name" Decode.string)
                        (field "schema" Decode.value)

                get sourceName path =
                    model.responses
                        |> Dict.get sourceName
                        |> (\s ->
                                case s of
                                    Nothing ->
                                        Encode.string ""

                                    Just resp ->
                                        Decode.decodeString (Decode.at path Decode.value) resp.body
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
                                        resp.body
                                            |> Decode.decodeString (Decode.at sourcePath <| Decode.list decodeService)
                                            |> Result.withDefault []
                           )

                set destName destPath x inputs =
                    inputs
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
                                model.inputs
                                    |> set "vault/create-pan" [ "otp" ] (get "vault/create-otp" [ "id" ])
                        }
                            ! []

                    "vault/create-pan" ->
                        let
                            panId =
                                get "vault/create-pan" [ "id" ]

                            key =
                                get "vault/create-pan" [ "key" ]
                        in
                        { model
                            | inputs =
                                model.inputs
                                    |> set "vault/create-fake-pan" [ "key" ] key
                                    |> set "vault/create-fake-pan" [ "panId" ] panId
                        }
                            ! []

                    "service/list-services" ->
                        { model
                            | services = getList "service/list-services" [ "data" ]
                        }
                            ! []

                    "service/create-job" ->
                        { model
                            | inputs =
                                model.inputs
                                    |> set "service/show-job" [ "id" ] (get "service/create-job" [ "id" ])
                        }
                            ! []

                    "service/show-job" ->
                        { model
                            | inputs =
                                model.inputs
                                    |> set "service/rfi-anwser" [ "id" ] (get "service/create-job" [ "rfiId" ])
                        }
                            ! []

                    _ ->
                        model ! []

        UpdateData reqType v ->
            { model | inputs = Dict.insert reqType v model.inputs } ! []

        UpdateJson reqType json ->
            case Decode.decodeString Decode.value json of
                Ok data -> 
                    { model | error = "", inputs = Dict.insert reqType data model.inputs } ! []
                Err error ->
                    { model | error = error } ! []

        EditAsJson reqType isJson ->
            { model | editAsJson = if isJson then
                Set.insert reqType model.editAsJson
            else
                Set.remove reqType model.editAsJson
                } ! []


codeStyle : Html.Attribute msg
codeStyle =
    style
        [ ( "background", "#444" )
        , ( "color", "#eee" )
        , ( "max-width", "300px" )
        , ( "overflow", "auto" )
        ]


renderResponseSchema : Schema -> Html.Html msg
renderResponseSchema schema =
    div [] <|
        JS.mapProperties schema.properties
            (\( name, prop ) ->
                div []
                    [ div
                        [ style
                            [ ( "border-top", "1px solid transparent" )
                            , ( "margin-top", "20px" )
                            , ( "padding", "0px" )
                            , ( "display", "flex" )
                            , ( "flex-direction", "row" )
                            ]
                        ]
                        [ div
                            [ style
                                [ ( "width", "38.2%" )
                                , ( "background", "rgba(210, 105, 30, 0.04)" )
                                , ( "flex-shrink", "0" )
                                , ( "text-align", "right" )
                                , ( "padding", "10px" )
                                , ( "box-sizing", "border-box" )
                                ]
                            ]
                            [ Html.code [ style [ ( "font-weight", "bold" ) ] ] [ text name ]
                            , Html.br [] []
                            , Html.span [ style [("color", "dimgrey")] ] [ text prop.type_ ]
                            ]
                        , div
                            [ style
                                [ ( "width", "auto" )
                                , ( "padding", "10px" )
                                ]
                            ]
                            [ Markdown.toHtml [ style [ ( "padding", "0" ) ], Attrs.class "markdown-doc" ] prop.description ]
                        ]
                    , case prop.items of
                        Just (JS.ArrayItemDefinition aid) ->
                            div [ style [ ( "padding-left", "0%" ) ] ] [ renderResponseSchema aid ]

                        Nothing ->
                            text ""
                    ]
            )


render : Model -> ClientSettings -> Html.Html Msg
render model clientSettings =
    let
        formatRequest name =
            let
                request =
                    findEndpoint name model.endpoints
            in
                case request of
                    Ok r ->
                        div [] [ renderRequest name r clientSettings ]

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

        renderRequest name r clientSettings =
            let
                headers =
                    buildHeaders r clientSettings
                        |> (::) ( "Host", r.service |> serviceUrl |> getHostname )
                        |> renderHeaders

                body =
                    if r.method == "GET" then
                        ""
                    else
                        case Dict.get name model.inputs of
                            Nothing ->
                                ""

                            Just val ->
                                encode 2 <| preprocess val r.pathname


                preprocess : Value -> String -> Value
                preprocess body pathnameTemplate =
                    pathnameTemplate
                        |> Regex.find Regex.All paramsMatcher
                        |> List.map .match
                        |> List.map (String.dropLeft 1)
                        |> List.foldl Dict.remove (objectBody body)
                        |> Dict.toList
                        |> Encode.object


                objectBody body =
                    body
                        |> Decode.decodeValue (Decode.keyValuePairs Decode.value)
                        |> Result.withDefault []
                        |> Dict.fromList

                paramsMatcher =
                    Regex.regex ":\\w+"

                pathname =
                    case Dict.get name model.inputs of
                        Nothing ->
                            r.pathname

                        Just val ->
                            interpolate r.pathname val

                interpolate str val =
                    Regex.replace Regex.All
                        paramsMatcher
                        (\{ match } ->
                            val
                                |> Decode.decodeValue (Decode.at [ String.dropLeft 1 match ] Decode.string)
                                |> Result.withDefault ""
                        )
                        str
            in
                "```http\n"
                    ++ r.method
                    ++ " "
                    ++ pathname
                    ++ " HTTP/1.1\n"
                    ++ headers
                    ++ "\n\n"
                    ++ body
                    ++ "\n```"
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
                |> Decode.decodeString Decode.value
                |> Result.withDefault (Encode.string "")
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
                                         , (if 200 <= resp.status.code && resp.status.code < 400 then
                                                "olivedrab"
                                            else
                                                "crimson"
                                           )
                                         )
                                       ]
                                )
                            ]
                            --[ text <| resp.url ++ " -- "
                            [ "```http\nHTTP/1.1 " ++ (toString resp.status.code) ++ " " ++ resp.status.message ++ "\n```" |> Markdown.toHtml [ style [ ( "padding", "0" ), ( "font-size", "12px" ) ] ]
                            ]
                        , resp.body |> renderJsonBody (resp.headers |> Dict.toList)
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

        renderBlock label buttonText name childNodes =
            let
                schema =
                    getSchema name model

                responseSchema =
                    getOutputSchema name model

                data =
                    model.inputs
                        |> Dict.get name
                        |> Maybe.withDefault null

                guide =
                    case Dict.get name model.endpoints of
                        Just endpoint ->
                            endpoint.description

                        Nothing ->
                            ""

                submitButtonStyle =
                    style
                        [ ( "font-family", "Iosevka, monospace" )
                        , ( "font-size", "14px" )
                        , ( "width", "61.8%" )
                        , ( "margin-left", "38.2%" )
                        , ( "height", "40px" )
                        , ( "background", "rgba(210, 105, 30, 0.80)" )
                        , ( "border-radius", "4px" )
                        , ( "border", "2px solid rgb(210, 105, 30)" )
                        , ( "font-weight", "bold" )
                        , ( "color", "white" )
                        ]
            in
                div [ style [ ( "border-bottom", "1px solid #aaa" ) ] ]
                    [ div [ style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
                        [ column "rgba(249, 245, 236, 0.58)"
                            "#282828"
                            "60%"
                            [ Html.form [ onSubmit (PerformRequest name) ]
                                [ Html.h3 [] [ text label ]
                                , Markdown.toHtml [] guide
                                , Html.h4 [] [ text "Request schema" ]
                                , if schema == JS.empty then
                                    text ""
                                else
                                    div [ style [ ( "padding", "10px" ) ] ]
                                        [ text "Edit mode: "
                                        , Html.span [ style [ ( "cursor", "pointer" ), ( "border", "1px solid #ccc" ), ( "background", "#eee" ), ( "padding", "5px" ) ], onClick <| EditAsJson name False ] [ text "form 🐌" ]
                                        , text  " "
                                        , Html.span [ style [ ( "cursor", "pointer" ), ( "border", "1px solid #ccc" ), ( "background", "#eee" ), ( "padding", "5px" ) ], onClick <| EditAsJson name True ] [ text "json 🐞" ]
                                        ]
                                , div [ style [ ( "margin-bottom", "10px" ) ] ]
                                    [ if schema == JS.empty then
                                        text "no data required"
                                    else if Set.member name model.editAsJson then
                                        Html.textarea
                                            [ onInput <| UpdateJson name
                                            , style
                                                [ ( "font-family", "Iosevka, Fira Code, Pragmata Pro, menlo, monospace" )
                                                , ( "font-size", "10px" )
                                                , ( "padding", "10px" )
                                                , ( "width", "100%" )
                                                , ( "box-sizing", "border-box" )
                                                , ( "height", "300px" )
                                                ]
                                            ] [
                                                text <| Encode.encode 2 data
                                                ]
                                    else
                                        FragForm.render
                                        { validationErrors = Dict.empty
                                        , schema = schema
                                        , data = data
                                        , onInput = UpdateData name
                                        }
                                    ]
                                , div []
                                    [ button
                                        [ Attrs.type_ "submit"
                                        , submitButtonStyle
                                        ]
                                        [ text buttonText ]
                                    ]
                                , Html.h4 [] [ text "Response schema" ]
                                , div [] [ renderResponseSchema responseSchema ]
                                , div [] childNodes
                                ]
                            ]
                        , column "#282828"
                            "cornsilk"
                            "40%"
                            [ Html.h3 [] [ text "Request" ]
                            , formatRequest name
                            , Html.h3 [] [ text "Response" ]
                            , formatResponse name
                            ]
                        ]
                    ]

        error =
            case model.error of
                "" ->
                    text ""

                _ ->
                    div
                        [ style
                            (boxStyle
                                ++ [ ( "border-color", "red" )
                                   , ( "position", "fixed" )
                                   , ( "background", "white" )
                                   , ( "top", "10px" )
                                   , ( "right", "10px" )
                                   ]
                            )
                        ]
                        [ text model.error ]
    in
        div []
            [ error
            , renderBlock
                "1. Request OTP to authorize saving PAN"
                "Create otp"
                "vault/create-otp"
                []
            , renderBlock
                "2. Save PAN"
                "Use otp -> create PAN"
                "vault/create-pan"
                []
            , renderBlock
                "3. Issue fake PAN given panId"
                "Exchange panId -> fake PAN"
                "vault/create-fake-pan"
                []
            , renderBlock
                "4. Fetch list of services"
                "Show me what you can do"
                "service/list-services"
                [ renderServices model SelectService ]
            , renderBlock
                "5. Submit job"
                "Do your job"
                "service/create-job"
                []
            , renderBlock
                "6. Poll for job status"
                "Poll"
                "service/show-job"
                []
            , renderBlock
                "7. Respond to RFI"
                "Answer"
                "service/rfi-answer"
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
