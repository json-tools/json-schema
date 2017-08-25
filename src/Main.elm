module Main exposing (main)

import Navigation exposing (Location, program, newUrl)
import Html exposing (Html)

type alias Model = {}


type Msg
    = NoOp
    | UrlChange Location


main : Program Never Model Msg
main =
    program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Location -> ( Model, Cmd Msg )
init location =
    {} ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    Html.text "Hello world :)"
