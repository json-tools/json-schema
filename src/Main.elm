port module Main exposing (main)

import Navigation exposing (Location, programWithFlags)
import Html exposing (Html)
import Html.Attributes
import Dict exposing (Dict)
import Set exposing (Set)
import StyleSheet exposing (Styles(None, Main, InlineError, SchemaHeader, JsonEditor, MenuItem, NoOutline, SourceCode), Variations(Active), stylesheet)
import Element.Events exposing (onClick, onMouseDown, onInput, onDoubleClick)
import Element.Attributes as Attributes exposing (center, vary, inlineStyle, spacing, padding, height, minWidth, width, yScrollbar, fill, px, percent)
import Element exposing (Element, el, row, text, column, paragraph, empty)
import Markdown
import Json.Decode as Decode exposing (Decoder, decodeString, decodeValue, Value)
import Json.Encode as Encode
import Json.Schema.Helpers
    exposing
        ( implyType
        , typeToString
        , setValue
        , deleteIn
        , for
        , whenObjectSchema
        , parseJsonPointer
        , makeJsonPointer
        , resolve
        , calcSubSchemaType
        , setPropertyName
        )
import Json.Schema.Examples exposing (coreSchemaDraft6, bookingSchema)
import Json.Schema.Definitions as Schema
    exposing
        ( Schema(BooleanSchema, ObjectSchema)
        , SubSchema
        , Schemata(Schemata)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType)
        , JsonValue(ObjectValue, ArrayValue, OtherValue)
        , jsonValueDecoder
        )


type alias View =
    Element Styles Variations Msg


type alias Model =
    { schema : Schema
    , value : Value
    , jsonValue : JsonValue
    , activeSection : String
    , editPaths : Set String
    , error : Maybe String
    , valueUpdateErrors : Dict String String
    , editPath : String
    , editValue : String
    , editPropertyName : String
    , dragOver : Bool
    }


type Msg
    = NoOp
    | UrlChange Location
    | StringChange String String
    | NumberChange String String
    | BooleanChange String Bool
    | ValueChange String String
    | DeleteMe String
    | ActiveSection String
    | EditSchema Value
    | DragOver Bool
    | ToggleEditing String
    | SetEditPath String Value
    | SetEditPropertyName String
    | SetPropertyName String
    | DownloadSchema


main : Program Value Model Msg
main =
    programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    Model
        coreSchemaDraft6
        -- value
        val
        -- jsonValue
        (val
            |> Decode.decodeValue jsonValueDecoder
            |> Result.withDefault (ObjectValue [])
        )
        -- activeSection
        location.hash
        -- editPaths
        Set.empty
        -- error
        Nothing
        -- valueUpdateErrors
        Dict.empty
        -- editPath
        ""
        -- editValue
        "null"
        -- editPropertyName
        ""
        -- dragOver
        False
        ! []



-- [ location.hash |> String.dropLeft 1 |> Dom.focus |> Task.attempt (\_ -> NoOp) ]


updateValue : Model -> String -> Result String Value -> Model
updateValue model path newStuff =
    let
        addError message =
            { model | valueUpdateErrors = model.valueUpdateErrors |> Dict.insert path message }

        update val =
            setValue model.value path val model.schema
    in
        case newStuff of
            Ok val ->
                case update val of
                    Err validationErrorMessage ->
                        addError validationErrorMessage

                    Ok v ->
                        { model | value = v, valueUpdateErrors = model.valueUpdateErrors |> Dict.remove path }

            Err s ->
                addError s


deletePath : Model -> String -> Model
deletePath model pointer =
    case deleteIn model.value pointer model.schema of
        Ok val ->
            { model | value = val }

        Err s ->
            let
                a =
                    Debug.log "err" s
            in
                model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetEditPath jsonPointer value ->
            { model
                | editPath = jsonPointer
                , editPropertyName = ""
                , editValue = value |> Encode.encode 2
            }
                ! []

        StringChange path str ->
            updateValue model path (str |> Encode.string |> Ok) ! []

        NumberChange path str ->
            updateValue model path (str |> String.toFloat |> Result.map Encode.float) ! []

        BooleanChange path bool ->
            updateValue model path (bool |> Encode.bool |> Ok) ! []

        ValueChange path str ->
            case decodeString Decode.value str of
                Ok _ ->
                    { model
                        | editValue = str
                        , valueUpdateErrors = model.valueUpdateErrors |> Dict.remove path
                    }
                        ! []

                Err s ->
                    let
                        a =
                            Debug.log "Oh no" s
                    in
                        { model | editValue = str, valueUpdateErrors = model.valueUpdateErrors |> Dict.insert path s } ! []

        --updateValue model path (str |> decodeString Decode.value) ! []
        UrlChange l ->
            { model | activeSection = l.hash } ! []

        ActiveSection s ->
            { model | activeSection = s } ! []

        EditSchema s ->
            { model
                | value = s
                , jsonValue =
                    s
                        |> Decode.decodeValue jsonValueDecoder
                        |> Result.withDefault (ObjectValue [])
            }
                ! []

        DragOver isOver ->
            { model | dragOver = isOver } ! []

        ToggleEditing p ->
            { model
                | editPaths =
                    if Set.member p model.editPaths then
                        Set.remove p model.editPaths
                    else
                        Set.insert p model.editPaths
            }
                ! []

        DeleteMe pointer ->
            deletePath model pointer ! []

        SetEditPropertyName pointer ->
            { model | editPropertyName = pointer, editPath = "" } ! []

        SetPropertyName str ->
            let
                newJsonPointer =
                    model.editPropertyName
                        |> parseJsonPointer
                        |> List.reverse
                        |> List.drop 1
                        |> (::) str
                        |> List.reverse
                        |> makeJsonPointer
            in
                { model
                    | value =
                        model.value
                            |> setPropertyName model.editPropertyName str model.schema
                            |> Result.withDefault model.value
                    , editPropertyName = newJsonPointer
                    , activeSection =
                        if model.activeSection == model.editPropertyName then
                            newJsonPointer
                        else
                            model.activeSection
                }
                    ! []

        DownloadSchema ->
            model ! [ download model.value ]


port activeSection : (String -> msg) -> Sub msg


port editSchema : (Value -> msg) -> Sub msg


port dragOver : (Bool -> msg) -> Sub msg


port download : Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ activeSection ActiveSection
        , editSchema EditSchema
        , dragOver DragOver
        ]


view : Model -> Html Msg
view model =
    let
        propertiesListing section fn =
            model.value
                |> Decode.decodeValue Schema.decoder
                |> Result.toMaybe
                |> Maybe.andThen
                    (\s ->
                        case s of
                            ObjectSchema os ->
                                Just os

                            _ ->
                                Nothing
                    )
                |> Maybe.andThen fn
                |> Maybe.withDefault (Schemata [])
                |> (\(Schemata props) ->
                        List.map
                            (\( key, _ ) ->
                                let
                                    thisSection =
                                        "#/" ++ section ++ "/" ++ key
                                in
                                    row None
                                        []
                                        [ key
                                            |> text
                                            |> el MenuItem
                                                [ vary Active <| thisSection == model.activeSection
                                                , padding 5
                                                , width <| fill 1
                                                ]
                                            |> Element.link thisSection
                                        ]
                            )
                            props
                   )
                |> column None [ padding 20 ]

        a =
            model.value
                |> Decode.decodeValue Schema.decoder
                |> Result.map (\schema -> documentation model 0 "#" schema model.schema)
                |> Result.withDefault empty

        {-
           b =
               coreSchemaDraft6
                   --|> Result.withDefault Encode.null
                   --|> Decode.decodeValue Schema.decoder
                   --|> Result.withDefault blankSchema
                   |>
                       Builder.encode 2
                   |> text
                   |> Element.node "pre"

           c =
               form model.valueUpdateErrors model.editPath model.editValue model.jsonValue []
        -}
        sectionSelected =
            model.activeSection
                |> String.split "/"
                |> List.drop 1
                |> List.filter ((/=) "")
                |> List.length
                |> (<) 0
    in
        Element.viewport stylesheet <|
            row Main
                [ height <| fill 1
                , width <| fill 1
                ]
            <|
                if model.dragOver then
                    [ row None [ center, Attributes.verticalCenter, width <| fill 1, height <| fill 1 ] [ text "Drop it!" ]
                    ]
                else
                    case model.jsonValue of
                        ObjectValue [] ->
                            [ row None [ center, Attributes.verticalCenter, width <| fill 1, height <| fill 1 ] [ text "Drop schema file here." ]
                            ]

                        _ ->
                            [ column None
                                [ height <| fill 1, minWidth <| px 200, yScrollbar, spacing 10, padding 0 ]
                                [ Element.bold "definitions"
                                , propertiesListing "definitions" .definitions
                                , Element.bold "properties"
                                , propertiesListing "properties" .properties
                                , Element.bold "misc"
                                , column None
                                    [ padding 20 ]
                                    [ el MenuItem [ onClick <| DownloadSchema ] <| text "download"
                                    , el MenuItem [] <| text "diff (WIP)"
                                    , el MenuItem [] <| text "reset (WIP)"
                                    , el MenuItem [] <| text "erase (WIP)"
                                    ]
                                ]
                            , column SourceCode
                                [ height <| fill 1
                                , width <| fill 1
                                , yScrollbar
                                , padding 10
                                , Attributes.id "content"
                                  -- , on "scroll" (Json.Decode.succeed ContentScroll)
                                ]
                                [ if sectionSelected then
                                    a
                                  else
                                    empty
                                ]
                              --[ Element.textLayout None [] c ]
                            ]


form : Dict String String -> String -> String -> String -> JsonValue -> List String -> List View
form valueUpdateErrors editPropertyName editPath editValue val path =
    let
        offset level n =
            el None
                [ inlineStyle
                    [ ( "display", "inline-block" )
                    , ( "margin-left", (level + n |> (*) 2 |> toString) ++ "ch" )
                    , ( "padding-left", "1ch" )
                    ]
                , Attributes.class "deletable"
                ]

        deleteMe path =
            Element.onLeft
                [ el None
                    [ Attributes.class "delete-me"
                    , Attributes.moveRight 5
                    ]
                  <|
                    el None [ onClick <| DeleteMe <| makeJsonPointer path ] <|
                        text "-"
                ]

        itemRow isEditableProp level path ( key, prop ) =
            let
                pp =
                    path ++ [ key ]

                newPointer =
                    makeJsonPointer pp

                propName =
                    if isEditableProp then
                        if newPointer == editPropertyName then
                            [ Element.inputText JsonEditor [ onInput <| SetPropertyName ] key
                            , text ":"
                            ]
                                |> row None []
                                |> offset level 1
                                |> deleteMe pp
                        else
                            toString key
                                ++ ": "
                                |> text
                                |> el None [ onClick <| SetEditPropertyName <| newPointer ]
                                |> offset level 1
                                |> deleteMe pp
                    else
                        el None [] <| text " "
            in
                if isEditableProp then
                    propName
                        :: text " "
                        :: controls (level + 1) prop pp
                else
                    controls (level + 1) prop pp
                        |> List.map
                            (\el ->
                                el
                                    |> offset level 1
                                    |> deleteMe pp
                            )

        joinWithCommaAndWrapWith open close isEditableProp level path list =
            list
                |> List.map (itemRow isEditableProp level path)
                |> List.intersperse [ text ",", Element.break ]
                |> List.concat
                |> (\x ->
                        text open
                            :: Element.break
                            :: (x ++ [ Element.break, offset level 0 <| text close ])
                   )

        controls level val path =
            case val of
                ArrayValue list ->
                    list
                        |> List.indexedMap (\index item -> ( toString index, item ))
                        |> joinWithCommaAndWrapWith "[" "]" False level path

                ObjectValue obj ->
                    obj
                        |> List.reverse
                        |> joinWithCommaAndWrapWith "{" "}" True level path

                OtherValue val ->
                    let
                        jsp =
                            makeJsonPointer path
                    in
                        if jsp == editPath then
                            [ editValue
                                |> Element.inputText JsonEditor
                                    [ onInput <| ValueChange jsp
                                      --, Attributes.contenteditable True
                                    , Attributes.autofocus True
                                    , Attributes.rows 1
                                    , inlineStyle [ ( "display", "inline-block" ) ]
                                    ]
                                |> Element.el None
                                    [ inlineStyle [ ( "display", "inline-block" ) ] ]
                                |> Element.below
                                    [ valueUpdateErrors
                                        |> Dict.get jsp
                                        |> Maybe.map (text >> (el InlineError []))
                                        |> Maybe.withDefault empty
                                    ]
                            ]
                        else
                            [ text (toString val)
                                |> Element.el None
                                    [ inlineStyle [ ( "display", "inline-block" ) ]
                                      --, Attributes.contenteditable False
                                    , onClick <| SetEditPath jsp val
                                    ]
                            ]
    in
        controls 0 val path



{- -}


getFields : Value -> String -> List ( String, Value )
getFields val subpath =
    let
        path =
            subpath
                |> String.split "/"
                |> List.drop 1
                |> List.filter ((/=) "")
    in
        val
            |> Decode.decodeValue (Decode.at path <| Decode.keyValuePairs Decode.value)
            |> Result.withDefault []
            |> List.reverse


col10 : List View -> View
col10 =
    column None [ spacing 10, padding 10 ]


source : Model -> Schema -> String -> View
source model s subpath =
    let
        isEditMode =
            True

        --Set.member subpath model.editPaths
        toggleEditingButton =
            column None
                [ Attributes.alignRight
                ]
                [ el None
                    [ onMouseDown <| ToggleEditing subpath
                    , inlineStyle [ ( "cursor", "pointer" ), ( "background", "rgba(255,255,255,0.5)" ), ( "color", "royalblue" ) ]
                    , padding 10
                    ]
                  <|
                    text
                        (if isEditMode then
                            ""
                            --"done editing"
                         else
                            "edit"
                        )
                ]

        val =
            s
                |> Schema.encode
                |> decodeValue jsonValueDecoder
                |> Result.withDefault (ObjectValue [])

        schemaSource val =
            val
                |> Encode.encode 2
                |> (\s -> "```json\n" ++ s ++ "```")
                |> Markdown.toHtml [ Html.Attributes.class "hljs" ]
                |> Element.html
                |> el SourceCode [ onDoubleClick <| ToggleEditing subpath ]

        editForm val =
            Element.textLayout None
                []
                (form model.valueUpdateErrors model.editPropertyName model.editPath model.editValue val (parseJsonPointer subpath)
                 --|> el SourceCode [ inlineStyle [ ( "margin", "5px" ) ] ]
                )

        displaySchemaNode =
            editForm
    in
        displaySchemaNode val
            |> Element.within [ toggleEditingButton ]


takeHalfWidth : View -> View
takeHalfWidth =
    el None [ width <| percent 50 ]


displayNextToEachOther : List View -> View
displayNextToEachOther list =
    row None
        [ Attributes.justify ]
        (list |> List.map takeHalfWidth)


displayOneUnderAnother : List View -> View
displayOneUnderAnother list =
    column None [] list


schemataDoc : Model -> Int -> Schema -> Maybe Schemata -> Schema -> String -> View
schemataDoc model level schema s metaSchema subpath =
    let
        schemataKey level parent s =
            let
                nodeName =
                    if level == 0 then
                        "h1"
                    else
                        "h2"
            in
                if model.editPropertyName == newSubpath s then
                    s
                        |> Element.inputText SchemaHeader
                            [ onInput <| SetPropertyName
                            , Attributes.autofocus True
                            ]
                else
                    s
                        |> text
                        |> el SchemaHeader
                            [ onDoubleClick <| SetEditPropertyName <| newSubpath s
                            ]
                        |> Element.node nodeName

        dropAnchor newSubpath =
            el NoOutline
                [ Attributes.tabindex 1
                , Attributes.id <| String.dropLeft 1 newSubpath
                ]

        newSubpath key =
            subpath ++ key

        heading key =
            schemataKey level subpath key
                |> dropAnchor (newSubpath key)

        {-
           let
               getSchema at =
                   model.value
                       |> Result.withDefault (Encode.object [])
                       |> Decode.decodeValue (Decode.at (parseJsonPointer at) Decode.value)
                       |> Result.andThen (Decode.decodeValue Schema.decoder)
                       |> Result.withDefault blankSchema

               rootSchema =
                   getSchema "#/"

               targetSchema =
                   getSchema jsonPointer
           in
               targetSchema
                   |> whenObjectSchema
                   |> Maybe.andThen (calcSubSchemaType Nothing rootSchema)
                   |> Maybe.map (\( t, _ ) -> typeToString t)
                   |> Maybe.withDefault "any"
        -}
        printProperty ( key, schema ) =
            if String.startsWith model.activeSection (newSubpath key) then
                case metaSchema |> for subpath of
                    Just ms ->
                        if level == 0 then
                            [ heading key
                              --, displayNextToEachOther
                            , documentation
                                model
                                (level + 1)
                                (newSubpath key)
                                schema
                                ms
                            ]
                                |> col10
                        else
                            [ heading key
                              --, displayOneUnderAnother
                            , documentation
                                model
                                (level + 1)
                                (newSubpath key)
                                schema
                                ms
                            ]
                                |> col10

                    Nothing ->
                        empty
            else
                empty

        printSchemata (Schemata s) =
            col10
                [ s
                    |> List.map printProperty
                    |> column None []
                ]
    in
        s
            |> Maybe.map printSchemata
            |> Maybe.withDefault empty


when : Maybe a -> (a -> View) -> View
when a fn =
    case a of
        Just s ->
            fn s

        Nothing ->
            empty


metaDoc : SubSchema -> View
metaDoc s =
    column None
        []
        [ when s.title (\title -> row None [ inlineStyle [ ( "font-size", "18px" ) ] ] [ title |> Element.bold ])
        , when s.description (\description -> paragraph None [ inlineStyle [ ( "font-size", "16px" ) ] ] [ description |> Markdown.toHtml [] |> Element.html ])
        ]



{-
   , if s.properties == Nothing && s.definitions == Nothing && Set.member jsonPointer model.editPaths then
       form model (schema |> Schema.encode) metaSchema jsonPointer ""
     else
       empty
-}
{-
   if Set.member jsonPointer model.editPaths then
       case schema of
           ObjectSchema s ->
               col10
                   [ metaDoc s
                   , form model (schema |> Schema.encode) metaSchema jsonPointer ""
                   ]

           _ ->
               empty
   else
-}


documentation : Model -> Int -> String -> Schema -> Schema -> View
documentation model level jsonPointer schema metaSchema =
    let
        generatedDocs s =
            col10
                [ metaDoc s
                , schemataDoc model level schema s.definitions metaSchema <| jsonPointer ++ "/definitions/"
                , schemataDoc model level schema s.properties metaSchema <| jsonPointer ++ "/properties/"
                ]

        schemaSource =
            source model schema jsonPointer
    in
        case schema of
            ObjectSchema s ->
                if level == 0 then
                    generatedDocs s
                else if level == 1 then
                    displayNextToEachOther
                        [ generatedDocs s
                        , schemaSource
                        ]
                else if level > 1 then
                    displayOneUnderAnother
                        [ generatedDocs s
                        , schemaSource
                        ]
                else
                    empty

            BooleanSchema b ->
                text <| toString b
