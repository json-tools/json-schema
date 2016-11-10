module Pages.Vault exposing (init, render, update, Msg, Model)

import HttpBuilder exposing (Error, Response)
import Models exposing (Otp, Pan, FakePan)
import Html exposing (text, div, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Task
import Services.Otp as OtpSvc
import Services.Pan as PanSvc
import Layout exposing (boxStyle)
import Types exposing (ClientSettings)

type alias Model =
    { otp : Maybe Otp
    , pan : Maybe Pan
    , fakePan : Maybe FakePan
    , error : String
    }

init : Model
init =
    Model
        -- otp
        Nothing
        -- pan
        Nothing
        -- fakePan
        Nothing
        -- error
        ""

type Msg
    = ResponseError (Error String)
    | CreateOtp
    | CreateOtpSuccess (Response Otp)
    | CreatePan
    | CreatePanSuccess (Response Pan)
    | CreateFakePan
    | CreateFakePanSuccess (Response FakePan)

update : Msg -> Model -> ClientSettings -> ( Model, Cmd Msg )
update msg model clientSettings =
    case msg of
        ResponseError e ->
            { model | error = toString e } ! []

        CreateOtp ->
            model ! [ Task.perform ResponseError CreateOtpSuccess <|
                OtpSvc.create clientSettings ]

        CreateOtpSuccess { data } ->
            { model | otp = Just data } ! []

        CreatePan ->
            case model.otp of
                Nothing ->
                    model ! []

                Just otp ->
                    model ! [ Task.perform ResponseError CreatePanSuccess <|
                        PanSvc.create otp "4111111111111111" clientSettings ]

        CreatePanSuccess { data } ->
            { model | pan = Just data } ! []

        CreateFakePan ->
            case model.pan of
                Nothing ->
                    model ! []

                Just pan ->
                    model ! [ Task.perform ResponseError CreateFakePanSuccess <|
                        PanSvc.createFake pan.id clientSettings ]

        CreateFakePanSuccess { data } ->
            { model | fakePan = Just data } ! []

render : Model -> ClientSettings -> Html.Html Msg
render model { guide, vault } =
    let
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
    in
        div []
            [ div [ style boxStyle ]
                [ text "1. Request one-time password to authenticate next request"
                , div [] [ button [ onClick CreateOtp ] [ text "Create OTP" ]]
                ]
            , div [ style boxStyle ]
                [ text "2. Save PAN"
                , div []
                    [ Html.pre [] [ text <| "POST " ++ vault ++ "/pan\n{ otp: '" ++ otpId ++ "' }" ]
                    ]
                , div [] [ button [ onClick CreatePan ] [ text "Create PAN" ] ]
                , text "card id: "
                , panId
                , Html.br [] []
                , text "decryption key: "
                , decryptionKey
                ]
            , div [ style boxStyle ]
                [ text "3. Issue fake card"
                , div [] [ button [ onClick CreateFakePan ] [ text "Create Fake PAN" ] ]
                , fakePan
                ]
            ]
