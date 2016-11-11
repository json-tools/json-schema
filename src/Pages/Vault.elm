module Pages.Vault exposing (init, render, update, Msg, Model)

import HttpBuilder exposing (Error, Response)
import Models exposing (Otp, Pan, FakePan)
import Html exposing (text, div, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Json.Encode exposing (encode, Value)
import Task
import Dict
import Services.Otp as OtpSvc
import Services.Pan as PanSvc
import Layout exposing (boxStyle)
import Types exposing (ClientSettings)
import Markdown

type alias Model =
    { responses : Dict.Dict String (Response Value)
    , error : String
    }

init : Model
init =
    Model
        -- responses
        Dict.empty
        -- error
        ""

type RequestType
    = CreateOtp
    | CreatePan Otp String
    --| CreateFakePan

type Msg
    = ResponseError String (Error Value)
    | ResponseSuccess String (Response Value)
    | PerformRequest RequestType

req : RequestType -> ClientSettings -> Task.Task (Error Value) (Response Value)
req reqType clientSettings =
    case reqType of
        CreateOtp ->
            OtpSvc.createRaw clientSettings

        CreatePan otp card ->
            PanSvc.createRaw otp card clientSettings

        -- CreateFakePan card ->

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
            let name =
                    case t of
                        CreateOtp -> "otp"
                        CreatePan a b -> "pan"
            in
            { model | responses = model.responses |> Dict.remove name } !
            [ Task.perform (ResponseError name) (ResponseSuccess name) <|
                req t clientSettings ]

        ResponseSuccess name resp ->
            { model | responses = model.responses |> Dict.insert name resp } ! []

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

        formatResponse name =
            case Dict.get name model.responses of
                Nothing ->
                    text "Push button to see response"

                Just resp ->
                    div []
                        [ div [] [ text resp.url ]
                        , text <| toString resp.status
                        , text <| " " ++ resp.statusText
                        , Html.dl [] (Dict.toList resp.headers
                            |> List.foldl (\(header, value) x ->
                                x ++
                                    [ Html.dt [] [ text header ]
                                    , Html.dd [] [ text value ]
                                    ]
                            ) []
                        )
                        , resp.data
                            |> encode 2
                            |> (\s -> "```json\n" ++ s ++ "\n```")
                            |> Markdown.toHtml []
                        ]
    in
        div []
            [ div [ style boxStyle ]
                [ text "1. Request one-time password to authenticate next request"
                , div [] [ button [ onClick (PerformRequest CreateOtp) ] [ text "Create OTP" ]]
                , formatResponse "otp"
                ]
            , div [ style boxStyle ]
                [ text "2. Save PAN"
                -- , div []
                    -- [ Html.pre [ codeStyle ] [ text <| "POST " ++ vault ++ "/pan\n{ otp: '" ++ otpId ++ "' }" ]
                    -- ]
                , div [] [ button [ onClick (PerformRequest <| CreatePan (Otp "") "4111111111111111") ] [ text "Create PAN" ] ]
                , formatResponse "pan"
                ]
            {-
            , div [ style boxStyle ]
                [ text "3. Issue fake card"
                , div [] [ button [ onClick CreateFakePan ] [ text "Create Fake PAN" ] ]
                , fakePan
                ]
            -}
            ]
