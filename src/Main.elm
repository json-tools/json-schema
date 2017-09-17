port module Main exposing (main)

import Navigation exposing (Location, programWithFlags)
import Html exposing (Html)
import Dom
import Task
import Dict exposing (Dict)
import StyleSheet
    exposing
        ( Styles
            ( None
            , Main
            , InlineError
            , SchemaHeader
            , JsonEditor
            , MenuItem
            , NoOutline
            , SourceCode
            , PropertyName
            , ItemIndex
            , PropertyValue
            , PropertySeparator
            )
        , Variations(Active)
        , stylesheet
        )
import Element.Events exposing (onClick, onMouseDown, onInput, onBlur, onFocus, onDoubleClick)
import Element.Attributes as Attributes exposing (center, vary, inlineStyle, spacing, padding, height, minWidth, width, yScrollbar, fill, px, percent)
import Element exposing (Element, el, row, text, column, paragraph, empty)
import Markdown
import Json.Decode as Decode exposing (Decoder, decodeString, decodeValue, Value)
import Json.Encode as Encode
import Json.Schema.Helpers
    exposing
        ( implyType
        , typeToString
        , setJsonValue
        , getJsonValue
        , deleteIn
        , for
        , whenObjectSchema
        , parseJsonPointer
        , makeJsonPointer
        , resolve
        , calcSubSchemaType
        , setPropertyNameInJsonValue
        )
import Validation
import Json.Schema.Examples exposing (coreSchemaDraft6, bookingSchema)
import Json.Schema.Definitions as Schema
    exposing
        ( Schema(BooleanSchema, ObjectSchema)
        , SubSchema
        , Schemata(Schemata)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType)
        , JsonValue(ObjectValue, ArrayValue, OtherValue, EmptyValue)
        , jsonValueDecoder
        , encodeJsonValue
        , blankSchema
        )


type alias View =
    Element Styles Variations Msg


type alias Model =
    { schema : Schema
    , value : Schema
    , jsonValue : JsonValue
    , activeSection : String
    , error : Maybe String
    , valueUpdateErrors : Dict String String
    , editPath : String
    , editValue : String
    , editPropertyName : ( String, Int )
    , dragOver : Bool
    }


type Msg
    = NoOp
    | UrlChange Location
      --| StringChange String String
      --| NumberChange String String
      --| BooleanChange String Bool
    | ValueChange String String
    | InsertValue Bool (List String) Int String
    | DeleteMe String
    | ActiveSection String
    | EditSchema Value
    | DragOver Bool
    | SetEditPath String String String
    | SetEditPropertyName String (List String) Int
    | StopEditing
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
        (val
            |> Decode.decodeValue Schema.decoder
            |> Result.withDefault blankSchema
        )
        -- jsonValue
        (val
            |> Decode.decodeValue jsonValueDecoder
            |> Result.withDefault (ObjectValue [])
        )
        -- activeSection
        location.hash
        -- error
        Nothing
        -- valueUpdateErrors
        Dict.empty
        -- editPath
        ""
        -- editValue
        "null"
        -- editPropertyName
        ( "", 0 )
        -- dragOver
        False
        ! []


focus : String -> Cmd Msg
focus id =
    if id /= "" then
        id
            |> Dom.focus
            |> Task.attempt (\_ -> NoOp)
    else
        Cmd.none


makeValidSchema : JsonValue -> Schema -> Result String Schema
makeValidSchema jsonValue schema =
    let
        val =
            jsonValue
                |> encodeJsonValue
    in
        schema
            |> Validation.validate val
            |> Result.map (\_ -> val)
            |> Result.andThen (Decode.decodeValue Schema.decoder)


updateValue : Model -> String -> Result String JsonValue -> Model
updateValue model path newStuff =
    let
        addError message =
            { model | valueUpdateErrors = model.valueUpdateErrors |> Dict.insert path message }
    in
        newStuff
            |> Result.andThen
                (\val ->
                    setJsonValue model.jsonValue path val
                )
            |> \res ->
                case res of
                    Ok v ->
                        case makeValidSchema v model.schema of
                            Ok x ->
                                { model
                                    | jsonValue = v
                                    , value = x
                                    , valueUpdateErrors = model.valueUpdateErrors |> Dict.remove path
                                }

                            Err message ->
                                { model
                                    | jsonValue = v
                                    , valueUpdateErrors = model.valueUpdateErrors |> Dict.insert path message
                                }

                    Err s ->
                        addError s


deletePath : Model -> String -> Model
deletePath model pointer =
    case deleteIn model.jsonValue pointer of
        Ok val ->
            { model | jsonValue = val }

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

        SetEditPath id jsonPointer value ->
            { model
                | editPath = jsonPointer
                , editPropertyName = ( "", 0 )
                , editValue = value
            }
                ! [ select id ]

        {-
           StringChange path str ->
               updateValue model path (str |> Encode.string |> OtherValue |> Ok) ! []

           NumberChange path str ->
               updateValue model path (str |> String.toFloat |> Result.map (Encode.float >> OtherValue)) ! []

           BooleanChange path bool ->
               updateValue model path (bool |> Encode.bool |> OtherValue |> Ok) ! []
        -}
        ValueChange path str ->
            updateValue { model | editValue = str, editPath = path } path (decodeString jsonValueDecoder str) ! []

        StopEditing ->
            { model
                | editPath = ""
                , editPropertyName = ( "", 0 )
                , jsonValue =
                    if model.editValue == "" && model.editPath /= "" then
                        deleteIn model.jsonValue model.editPath
                            |> Result.withDefault model.jsonValue
                    else
                        model.jsonValue
            }
                ! []

        InsertValue hasKey path index formId ->
            let
                newJsonPointer =
                    makeJsonPointer path

                ( editProp, editPath, id ) =
                    if hasKey then
                        ( newJsonPointer, "", formId ++ "/prop/" ++ newJsonPointer ++ "/" )
                    else
                        ( "", newJsonPointer ++ "/" ++ (toString index), formId ++ "/value/" ++ newJsonPointer ++ "/" ++ (toString index) )
            in
                updateValue
                    { model
                        | editPath = editPath
                        , editPropertyName = ( editProp, index )
                        , editValue = ""
                    }
                    (newJsonPointer ++ "/")
                    (Ok <| EmptyValue)
                    ! [ select id ]

        UrlChange l ->
            { model | activeSection = l.hash } ! []

        ActiveSection s ->
            { model | activeSection = s } ! []

        EditSchema s ->
            { model
                | value =
                    s
                        |> Decode.decodeValue Schema.decoder
                        |> Result.withDefault model.value
                , jsonValue =
                    s
                        |> Decode.decodeValue jsonValueDecoder
                        |> Result.withDefault (ObjectValue [])
            }
                ! []

        DragOver isOver ->
            { model | dragOver = isOver } ! []

        DeleteMe pointer ->
            deletePath model pointer ! []

        SetEditPropertyName id path index ->
            { model | editPropertyName = ( makeJsonPointer path, index ), editPath = "" } ! [ select id ]

        SetPropertyName str ->
            --let
            {-
               newJsonPointer =
                   model.editPropertyName
                       |> parseJsonPointer
                       |> List.reverse
                       |> (::) str
                       |> List.reverse
                       |> makeJsonPointer
            -}
            --in
            { model
                | jsonValue =
                    model.jsonValue
                        |> setPropertyNameInJsonValue model.editPropertyName str
                        |> Result.withDefault model.jsonValue
                    {-
                       , activeSection =
                           if model.activeSection == model.editPropertyName then
                               newJsonPointer
                           else
                               model.activeSection
                    -}
            }
                ! []

        DownloadSchema ->
            model ! [ model.value |> Schema.encode |> download ]


port activeSection : (String -> msg) -> Sub msg


port editSchema : (Value -> msg) -> Sub msg


port dragOver : (Bool -> msg) -> Sub msg


port download : Value -> Cmd msg


port select : String -> Cmd msg


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
            Just model.value
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
            documentation model 0 "#" model.value model.schema

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
                            [ row None [ center, Attributes.verticalCenter, width <| fill 1, height <| fill 1 ] [ text "Drop schema file (or link to a schema) here." ]
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


form : String -> Dict String String -> ( String, Int ) -> String -> String -> JsonValue -> List String -> List View
form id valueUpdateErrors editPropertyName editPath editValue val path =
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

        itemRow isEditableProp level path ( index, key, prop ) =
            let
                pp =
                    path
                        ++ [ if isEditableProp then
                                key
                             else
                                index |> toString
                           ]

                hostPointer =
                    makeJsonPointer path

                ( editPropPath, editIndex ) =
                    editPropertyName

                newPointer =
                    makeJsonPointer pp

                propId =
                    id ++ "/prop/" ++ newPointer

                propName =
                    if isEditableProp then
                        if hostPointer == editPropPath && index == editIndex then
                            key
                                |> Element.inputText JsonEditor
                                    [ onInput <| SetPropertyName
                                    , Attributes.size <| String.length key + 1
                                    , onBlur <| SetEditPropertyName "" [] 0
                                    , Attributes.tabindex 0
                                    , Attributes.id propId
                                    ]
                                |> el None []
                        else
                            key
                                |> text
                                |> el PropertyName
                                    [ Attributes.tabindex 0
                                    , onFocus <| SetEditPropertyName propId path index
                                    ]
                    else
                        index
                            |> toString
                            |> text
                            |> el ItemIndex []
            in
                (propName
                    |> offset level 1
                    |> deleteMe pp
                )
                    :: (el PropertySeparator [ inlineStyle [ ( "display", "inline-block" ), ( "padding-right", "1ch" ) ] ] <| text ":")
                    :: controls (level + 1) prop pp

        joinWithCommaAndWrapWith open close isEditableProp level path list =
            list
                |> List.map (itemRow isEditableProp level path)
                |> List.intersperse [ text ",", Element.break ]
                |> List.concat
                |> (\x ->
                        (el PropertySeparator
                            [ inlineStyle
                                [ ( "padding-left"
                                  , if level == 0 then
                                        "1ch"
                                    else
                                        "0"
                                  )
                                , ( "display", "inline-block" )
                                ]
                            ]
                         <|
                            text open
                        )
                            :: Element.break
                            :: (x
                                    ++ [ Element.break
                                       , offset level 0 <|
                                            el PropertySeparator
                                                (if editPath == "" || editValue /= "" then
                                                    [ onFocus <| InsertValue isEditableProp path (List.length list) id
                                                    , Attributes.tabindex 0
                                                    ]
                                                 else
                                                    []
                                                )
                                            <|
                                                text close
                                       ]
                               )
                   )

        controls level val path =
            let
                edit val =
                    let
                        jsp =
                            makeJsonPointer path

                        valId =
                            id ++ "/value/" ++ jsp
                    in
                        if jsp == editPath then
                            [ editValue
                                |> Element.inputText JsonEditor
                                    [ onInput <| ValueChange jsp
                                    , onBlur StopEditing
                                    , Attributes.size <| String.length editValue + 1
                                    , inlineStyle [ ( "display", "inline-block" ) ]
                                    , Attributes.tabindex 0
                                    , Attributes.id valId
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
                            [ val
                                |> (\v ->
                                        if v == "" then
                                            "∅"
                                        else
                                            v
                                   )
                                |> text
                                |> Element.el PropertyValue
                                    [ inlineStyle [ ( "display", "inline-block" ) ]
                                      --, Attributes.contenteditable False
                                    , onFocus <| SetEditPath valId jsp val
                                    , Attributes.tabindex 0
                                    ]
                            ]
            in
                case val of
                    ArrayValue list ->
                        list
                            |> List.indexedMap (\index item -> ( index, "", item ))
                            |> joinWithCommaAndWrapWith "[" "]" False level path

                    ObjectValue obj ->
                        obj
                            |> List.indexedMap (\index ( key, val ) -> ( index, key, val ))
                            |> joinWithCommaAndWrapWith "{" "}" True level path

                    EmptyValue ->
                        edit ""

                    OtherValue val ->
                        val |> Encode.encode 2 |> edit
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


source : String -> Model -> Schema -> String -> View
source id model s subpath =
    let
        isEditMode =
            True

        val =
            model.jsonValue
                |> getJsonValue (parseJsonPointer subpath)
                |> Result.withDefault (ObjectValue [])

        editForm val =
            Element.textLayout None
                []
                (subpath
                    |> parseJsonPointer
                    |> form
                        id
                        model.valueUpdateErrors
                        model.editPropertyName
                        model.editPath
                        model.editValue
                        val
                )

        displaySchemaNode =
            editForm
    in
        displaySchemaNode val


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
                {-
                   if model.editPropertyName == newSubpath s then
                       s
                           |> Element.inputText SchemaHeader
                               [ onInput <| SetPropertyName
                               , Attributes.autofocus True
                               ]
                   else
                -}
                s
                    |> text
                    |> el SchemaHeader
                        [ onDoubleClick <| SetEditPropertyName "" [] 0
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
            if model.activeSection == (newSubpath key) then
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


documentation : Model -> Int -> String -> Schema -> Schema -> View
documentation model level jsonPointer schema metaSchema =
    let
        generatedDocs s =
            col10
                [ metaDoc s
                , schemataDoc model level schema s.definitions metaSchema <| jsonPointer ++ "/definitions/"
                , schemataDoc model level schema s.properties metaSchema <| jsonPointer ++ "/properties/"
                ]

        schemaSource id =
            source id model schema jsonPointer
    in
        case schema of
            ObjectSchema s ->
                if level == 0 then
                    generatedDocs s
                else if level == 1 then
                    displayNextToEachOther
                        [ generatedDocs s
                        , schemaSource "root"
                        ]
                else if level > 1 then
                    displayOneUnderAnother
                        [ generatedDocs s
                        , schemaSource "deep"
                        ]
                else
                    empty

            BooleanSchema b ->
                text <| toString b
