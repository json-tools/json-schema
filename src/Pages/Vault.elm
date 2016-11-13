module Pages.Vault exposing (init, render, update, Msg, Model)

import Fragments.Form as FragForm
import HttpBuilder exposing (Error, Response)
import Html exposing (text, div, button)
import Html.Events exposing (onClick, onSubmit)
import Html.Attributes as Attrs exposing (style)
import Json.Encode as Encode exposing (encode, null)
import Json.Decode as Decode exposing (value, Value)
import Task
import Dict
import Regex
import String
import Services.Otp as OtpSvc
import Services.Pan as PanSvc
import Services.ServiceDescriptor as ServiceDescriptorSvc
import Layout exposing (boxStyle)
import Types exposing (ClientSettings)
import Markdown
import JsonSchema as JS exposing (Schema)


type alias Model =
    { responses : Dict.Dict String (Response Value)
    , inputs : Dict.Dict String Value
    , error : String
    }


schema : String -> Schema
schema str =
    JS.fromString str |> Result.withDefault JS.empty


panSchema : Schema
panSchema =
    schema """
        { "type": "object"
        , "properties":
            { "otp":
                { "type": "string"
                }
            , "pan":
                { "type": "string"
                }
            }
        , "required": [ "pan", "otp" ]
        }
    """


fakePanSchema : Schema
fakePanSchema =
    schema """
        { "type": "object"
        , "properties":
            { "panId":
                { "type": "string"
                }
            }
        , "required": [ "panId" ]
        }
    """


init : Model
init =
    Model
        -- responses
        Dict.empty
        -- inputs
        Dict.empty
        -- error
        ""


type RequestType
    = CreateOtp
    | CreatePan
    | CreateFakePan
    | FetchServices

type PostAction
    = NoOp
    | FillPan
    | FillFake

type Msg
    = ResponseError String (Error Value)
    | ResponseSuccess String (Response Value)
    | PerformRequest RequestType
    | UpdateData RequestType Value
    | PerformPostAction String


req : RequestType -> Dict.Dict String Value -> ClientSettings -> HttpBuilder.RequestBuilder
req reqType inputs =
    let
        data =
            inputs
                |> Dict.get (requestName reqType)
                |> Maybe.withDefault null
    in
        case reqType of
            CreateOtp ->
                OtpSvc.createRequest data

            CreatePan ->
                PanSvc.createRequest data

            CreateFakePan ->
                PanSvc.createFakeRequest data

            FetchServices ->
                ServiceDescriptorSvc.listRequest data


requestName : RequestType -> String
requestName t =
    case t of
        CreateOtp ->
            "otp"

        CreatePan ->
            "pan"

        CreateFakePan ->
            "fake-pan"

        FetchServices ->
            "services"

getSchema : String -> Schema
getSchema t =
    case t of
        "pan" ->
            panSchema

        "fake-pan" ->
            fakePanSchema

        _ -> JS.empty


send : HttpBuilder.RequestBuilder -> Task.Task (Error Value) (Response Value)
send =
    HttpBuilder.send
        (HttpBuilder.jsonReader value)
        (HttpBuilder.jsonReader value)


update : Msg -> Model -> ClientSettings -> ( Model, Cmd Msg )
update msg model clientSettings =
    case msg of
        ResponseError name e ->
            case e of
                HttpBuilder.BadResponse resp ->
                    { model | responses = model.responses |> Dict.insert name resp } ! []

                _ ->
                    { model | error = toString e } ! []

        PerformRequest t ->
            let
                name =
                    requestName t

                updatedModel =
                    model
                    -- { model | responses = model.responses |> Dict.remove name }
            in
                updatedModel
                    ! [ Task.perform (ResponseError name) (ResponseSuccess name) <|
                            send (req t model.inputs clientSettings)
                      ]

        ResponseSuccess name resp ->
            let
                updatedModel =
                    { model | responses = model.responses |> Dict.insert name resp }
            in
                update (PerformPostAction name) updatedModel clientSettings

        PerformPostAction name ->
            let
                get sourceName sourcePath =
                    model.responses
                        |> Dict.get sourceName
                        |> (\s ->
                            case s of
                                Nothing -> Encode.string ""
                                Just resp ->
                                    Decode.decodeValue (Decode.at sourcePath Decode.value) resp.data
                                        |> Result.withDefault (Encode.string "")
                           )

                set destName destPath x =
                    model.inputs
                        |> Dict.update destName (\v ->
                            case v of
                                Nothing ->
                                    Just <| JS.setValue (getSchema destName) destPath x null

                                Just val ->
                                    Just <| JS.setValue (getSchema destName) destPath x val
                            )
            in
                case name of
                    "otp" ->
                        { model | inputs =
                            get "otp" [ "id" ]
                                |> set "pan" [ "otp" ]
                        } ! []

                    "pan" ->
                        { model | inputs =
                            get "pan" [ "id" ]
                                |> set "fake-pan" [ "panId" ]
                        } ! []

                    _ ->
                        model ! []

        UpdateData reqType v ->
            { model | inputs = Dict.insert (requestName reqType) v model.inputs } ! []


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
        {-
           otpId =
               case model.otp of
                   Nothing ->
                       ""

                   Just otp ->
                       otp.id

           panId =
               case model.pan of
                   Nothing ->
                       text ""

                   Just pan ->
                       text pan.id

           decryptionKey =
               case model.pan of
                   Nothing ->
                       text ""

                   Just pan ->
                       text pan.key

           fakePan =
               case model.fakePan of
                   Nothing ->
                       text ""

                   Just fakePan ->
                       text fakePan
        -}
        convertBodyToValue body =
            let
                str =
                    body |> toString

                match =
                    str
                        |> Regex.find (Regex.AtMost 1) (Regex.regex "^BodyString")
                        |> List.length
            in
                if match == 1 then
                    case (String.dropLeft 11 str |> Decode.decodeString Decode.string) of
                        Ok str ->
                            str
                                |> Decode.decodeString value
                                |> Result.withDefault (Encode.string str)

                        Err str ->
                            Encode.string str
                else
                    Encode.string str

        formatRequest kind =
            let
                request =
                    req kind model.inputs clientSettings
                        |> HttpBuilder.toRequest
            in
                div []
                    [ request.verb ++ " " ++ request.url |> text |> (\s -> [ s ]) |> div [ style (boxStyle ++ [ ( "margin", "0 0 10px 0" ), ( "border-color", "#928374" ), ( "background", "#333" ) ]) ]
                    , request.body |> convertBodyToValue |> renderJsonBody request.headers
                    ]

        renderHeaders h =
            List.map (\( header, value ) -> header ++ ": " ++ value) h
                |> String.join "\n"

        renderJsonBody headers data =
            data
                |> encode 2
                |> (\s -> "```http\n" ++ (renderHeaders headers) ++ "\n\n" ++ s ++ "\n```")
                |> Markdown.toHtml
                    [ style
                        (boxStyle
                            ++ [ ( "margin", "0 0 10px 0" )
                               , ( "background", "#333" )
                               , ( "color", "#ddd" )
                               , ( "font-size", "12px" )
                               , ( "line-height", "1.2em" )
                               , ( "border-color", "#928374" )
                               , ( "max-height", "500px" )
                               ]
                        )
                    ]

        formatResponse requestType =
            case Dict.get (requestName requestType) model.responses of
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
                    --, ( "transition", "height 1s 1s" )
                    ]
                ]

        renderBlock label buttonText requestBuilder postAction =
            let
                name =
                    requestName requestBuilder

                schema =
                    getSchema name

                data =
                    model.inputs
                        |> Dict.get name
                        |> Maybe.withDefault null
            in
                div [ style [ ( "border-bottom", "1px solid #aaa" ) ] ]
                    [ div [ style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
                        [ column "transparent" "black" "34%"
                            [ Html.form [ onSubmit (PerformRequest requestBuilder ) ]
                                [ text label
                                , FragForm.render
                                    { validationErrors = Dict.empty
                                    , schema = schema
                                    , data = data
                                    , onInput = UpdateData requestBuilder
                                    }
                                , div [] [ button [ Attrs.type' "submit" ] [ text buttonText ] ]
                                ]
                            ]
                        , column "#444" "cornsilk" "33%"
                            [ Html.h3 [] [ text "Request" ]
                            , formatRequest requestBuilder
                            ]
                        , column "#222" "cornsilk" "33%"
                            [ Html.h3 [] [ text "Response" ]
                            , formatResponse requestBuilder
                            ]
                        ]
                    ]
    in
        div []
            [ renderBlock
                "1. Request OTP to authorize saving PAN"
                "Create OTP"
                CreateOtp
                FillPan
            , renderBlock
                "2. Save PAN"
                "Create PAN"
                CreatePan
                FillFake
            , renderBlock
                "3. Issue fake card"
                "Create Fake PAN"
                CreateFakePan
                NoOp
            , renderBlock
                "4. Do something else"
                "Just do it"
                FetchServices
                NoOp
            ]
