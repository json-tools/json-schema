module Pages.Schema exposing (render, Msg, update, Model, init)

import Types exposing (..)
import Models exposing (ValidationErrors)
import Json.Encode as Encode exposing (Value)
import Html exposing (div, span, button, text, input, ul, li)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes as Attrs exposing (style)
import Dict
import Set
import String
import JsonSchema as JS
import Layout exposing (boxStyle)


type alias Path =
    List String


type Msg
    = NoOp
    | UpdateProperty Model Path Value


type alias Model =
    { validationErrors : ValidationErrors
    , schema : Schema
    , data : Value
    }


init : Model
init =
    Model
        -- validationErrors
        Dict.empty
        -- schema
        JS.empty
        -- data
        (Encode.object [])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        UpdateProperty ctx path val ->
            { model | data = JS.setValue ctx.schema path val ctx.data } ! []


render : Model -> Html.Html Msg
render context =
    renderSchema context [] context.schema


renderSchema : Model -> Path -> Schema -> Html.Html Msg
renderSchema context path node =
    let
        renderRow : ( String, Schema ) -> Html.Html Msg
        renderRow ( name, property ) =
            let
                required =
                    Set.member name node.required

                newPath =
                    path ++ [ name ]

                validationError =
                    Dict.get newPath context.validationErrors
                        |> Maybe.withDefault ""

                hasError =
                    Dict.member newPath context.validationErrors

                rowStyle =
                    if hasError then
                        boxStyle ++ [ ( "border-color", "red" ) ]
                    else
                        boxStyle
            in
                div [ style rowStyle ]
                    [ text
                        (if required then
                            "* "
                         else
                            ""
                        )
                    , text (name ++ ": ")
                    , renderProperty context property required newPath
                    , if hasError then
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
                      else
                        text ""
                    ]
    in
        div [] <| JS.mapProperties node.properties renderRow


renderSelect : Model -> List String -> Schema -> Bool -> Path -> Html.Html Msg
renderSelect context options prop required path =
    options
        |> List.map (\opt -> Html.option [] [ text opt ])
        |> Html.select
            [ Html.Events.onInput (\s -> UpdateProperty context path <| Encode.string s)
            , Attrs.value <| JS.getString context.schema path context.data
            ]


renderProperty : Model -> Schema -> Bool -> Path -> Html.Html Msg
renderProperty context prop required path =
    case prop.type_ of
        "string" ->
            case prop.enum of
                Nothing ->
                    renderInput context prop required path

                Just enum ->
                    renderSelect context enum prop required path

        "integer" ->
            renderInput context prop required path

        "boolean" ->
            renderInput context prop required path

        "object" ->
            renderSchema context path prop

        "array" ->
            case prop.items of
                Just (JS.ArrayItemDefinition itemDefinition) ->
                    renderArray context itemDefinition required path

                Nothing ->
                    text "missing item definition for array"

        _ ->
            text ("Unknown property type: " ++ prop.type_)


renderArray : Model -> Schema -> Bool -> List String -> Html.Html Msg
renderArray context property required path =
    let
        length =
            JS.getLength context.schema path context.data

        buttonStyle =
            [ ( "background", "white" )
            , ( "cursor", "pointer" )
            , ( "border", "1px solid ActiveBorder" )
            , ( "color", "ActiveBorder" )
            , ( "margin", "10px" )
            , ( "padding", "5px" )
            , ( "display", "inline-block" )
            ]

        renderItem index =
            div [ style boxStyle ]
                [ text ("#" ++ index)
                , renderProperty
                    context
                    property
                    required
                    (path ++ [ index ])
                ]
    in
        div []
            [ div [] <|
                List.map renderItem <|
                    List.map toString (List.range 0 (length - 1))
            , span
                [ onClick (UpdateProperty context (path ++ [ toString length ]) (JS.defaultFor property))
                , style buttonStyle
                ]
                [ text "Add item" ]
            ]


renderInput : Model -> Schema -> Bool -> Path -> Html.Html Msg
renderInput context property required path =
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

                        "boolean" ->
                            "checkbox"

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

        update s =
            UpdateProperty context
                path
                (case property.type_ of
                    "integer" ->
                        Encode.int (Result.withDefault 0 (String.toInt s))

                    _ ->
                        Encode.string s
                )
    in
        input
            [ Attrs.required required
              -- , Attrs.name name
            , Attrs.title title
            , Attrs.pattern pattern
            , Attrs.type_ inputType
            , onInput update
            , style
                [ ( "font-family", "iosevka, menlo, monospace" )
                , ( "min-width", "90%" )
                , ( "font-size", "12px" )
                , ( "padding", "3px" )
                ]
            , Attrs.value <|
                if property.type_ == "integer" then
                    JS.getInt context.schema path context.data |> toString
                else
                    JS.getString context.schema path context.data
            ]
            []
