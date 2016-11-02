port module Main exposing (..)

import Html exposing (div, text)
import Html.App exposing (program)


-- import Html.Events
-- import Html.Attributes
-- import Dict

import Http
import Task exposing (Task)
import Json.Decode as Decode exposing (Decoder, succeed, maybe, string, (:=))
import Json.Decode.Extra exposing ((|:))


main : Program Never
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { schema : Schema
    , error : String
    }


type alias Schema =
    { id : String
    , title : Maybe String
    }



--, definitions: Dict String Definition }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Schema "" Nothing)
        ""
    , fetchCmd
    )


schema : Decoder Schema
schema =
    succeed Schema
        |: ("id" := string)
        |: (maybe ("title" := string))


url : String
url =
    --    "https://raw.githubusercontent.com/universalbasket/service-api/master/docs/schema.json?token=AALPbLxstXdRX4D_d8u0_IA09oGZPgQrks5YIPKzwA%3D%3D"
    "http://json-schema.org/schema"


fetchTask : Task Http.Error Schema
fetchTask =
    Http.get schema url


fetchCmd : Cmd Msg
fetchCmd =
    Task.perform FetchError FetchSuccess fetchTask



-- UPDATE


type Msg
    = FetchError Http.Error
    | FetchSuccess Schema


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchError err ->
            { model | error = toString err } ! []

        FetchSuccess schema ->
            { model | schema = schema } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.none
        ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    div []
        [ text model.schema.id
        , text model.error
        ]
