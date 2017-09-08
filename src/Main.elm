port module Main exposing (main)

import Navigation exposing (Location, program, newUrl)
import Html exposing (Html)
import Html.Attributes
import Dict exposing (Dict)
import Set exposing (Set)
import StyleSheet
    exposing
        ( Styles
            ( None
            , Main
            , Error
            , InlineError
            , SchemaHeader
            , JsonEditor
            , MenuItem
            , NoOutline
            , Bordered
            , SourceCode
            )
        , Variations(Active)
        , stylesheet
        )
import Element.Events exposing (on, onClick, onMouseDown, onMouseOver, onMouseOut, onInput, onCheck, onDoubleClick)
import Element.Attributes as Attributes exposing (vary, inlineStyle, spacing, padding, alignLeft, height, minWidth, maxWidth, width, yScrollbar, fill, px, percent)
import Element exposing (Element, el, row, text, column, paragraph, empty)
import Markdown
import Json.Decode as Decode exposing (decodeString, decodeValue, Value)
import Json.Encode as Encode
import Json.Schema.Helpers exposing (ImpliedType, implyType, typeToString, setValue, for, whenObjectSchema, parseJsonPointer, resolve, calcSubSchemaType)
import Json.Schema.Examples exposing (coreSchemaDraft6, bookingSchema)
import Json.Schema.Builder as Builder
import Json.Schema.Definitions as Schema
    exposing
        ( Schema(BooleanSchema, ObjectSchema)
        , SubSchema
        , Schemata(Schemata)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        , blankSubSchema
        , blankSchema
        )


type alias View =
    Element Styles Variations Msg


type alias Model =
    { schema : Schema
    , value : Result String Value
    , activeSection : String
    , editPaths : Set String
    , error : Maybe String
    , valueUpdateErrors : Dict String String
    }


type Msg
    = NoOp
    | UrlChange Location
    | StringChange String String
    | NumberChange String String
    | BooleanChange String Bool
    | ValueChange String String
    | ContentScroll
    | ActiveSection String
    | ToggleEditing String


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
    Model
        coreSchemaDraft6
        (bookingSchema |> Schema.encode |> Ok)
        -- activeSection
        location.hash
        -- editPaths
        Set.empty
        -- error
        Nothing
        -- valueUpdateErrors
        Dict.empty
        ! []



-- [ location.hash |> String.dropLeft 1 |> Dom.focus |> Task.attempt (\_ -> NoOp) ]


updateValue : Model -> String -> Result String Value -> Model
updateValue model path newStuff =
    let
        addError message =
            { model | valueUpdateErrors = model.valueUpdateErrors |> Dict.insert path message }

        update : Value -> Result String Value
        update val =
            (Ok model.schema)
                |> Result.map2 (,) model.value
                |> Result.andThen (\( v, s ) -> setValue v path val s)
    in
        case newStuff of
            Ok val ->
                case update val of
                    Err validationErrorMessage ->
                        addError validationErrorMessage

                    Ok v ->
                        { model | value = Ok v, valueUpdateErrors = model.valueUpdateErrors |> Dict.remove path }

            Err s ->
                addError s


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StringChange path str ->
            updateValue model path (str |> Encode.string |> Ok) ! []

        NumberChange path str ->
            updateValue model path (str |> String.toFloat |> Result.map Encode.float) ! []

        BooleanChange path bool ->
            updateValue model path (bool |> Encode.bool |> Ok) ! []

        ValueChange path str ->
            updateValue model path (str |> decodeString Decode.value) ! []

        UrlChange l ->
            { model | activeSection = l.hash } ! []

        ActiveSection s ->
            { model | activeSection = s } ! []

        ToggleEditing p ->
            { model
                | editPaths =
                    if Set.member p model.editPaths then
                        Set.remove p model.editPaths
                    else
                        Set.insert p model.editPaths
            }
                ! []

        _ ->
            model ! []


port activeSection : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    activeSection ActiveSection


view : Model -> Html Msg
view model =
    let
        propertiesListing section fn =
            model.value
                |> Result.andThen (decodeValue Schema.decoder)
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
                |> Result.andThen (decodeValue Schema.decoder)
                |> Result.map (\schema -> documentation model 0 "#" schema model.schema)
                |> Result.withDefault empty

        {-
           b =
              [ model.value
                  |> Result.withDefault Encode.null
                  |> Decode.decodeValue Schema.decoder
                  |> Result.withDefault blankSchema
                  |> Builder.encode 2
                  |> text
                  |> Element.node "pre"
        -}
    in
        Element.viewport stylesheet <|
            row Main
                [ height <| fill 1
                , width <| fill 1
                ]
                [ column None
                    [ height <| fill 1, minWidth <| px 200, yScrollbar, spacing 10, padding 0 ]
                    [ Element.bold "definitions"
                    , propertiesListing "definitions" .definitions
                    , Element.bold "properties"
                    , propertiesListing "properties" .properties
                    ]
                , column None
                    [ height <| fill 1
                    , width <| fill 1
                    , yScrollbar
                    , padding 10
                    , Attributes.id "content"
                      -- , on "scroll" (Json.Decode.succeed ContentScroll)
                    ]
                    [ a
                    ]
                ]


form : Model -> Value -> Schema -> String -> String -> View
form model val schema subpath key =
    let
        path =
            subpath
                |> String.split "/"
                |> List.drop 1
                |> List.filter ((/=) "")
                |> (\s -> s ++ [ key ])

        jsonPointer =
            path
                |> String.join "/"
                |> (\x -> "#/" ++ x)

        implied =
            implyType val schema jsonPointer

        controls =
            case implied.type_ of
                SingleType StringType ->
                    val
                        |> Decode.decodeValue (Decode.at path Decode.string)
                        |> (\s ->
                                case s of
                                    Ok s ->
                                        case implied.schema.enum of
                                            Nothing ->
                                                Element.inputText None [ onInput <| StringChange jsonPointer ] s

                                            Just enum ->
                                                enum
                                                    |> List.map
                                                        (\v ->
                                                            v
                                                                |> Decode.decodeValue Decode.string
                                                                |> Result.withDefault ""
                                                                |> (\val ->
                                                                        Element.option val (s == val) (text val)
                                                                   )
                                                        )
                                                    |> Element.select ""
                                                        None
                                                        [ onInput <| StringChange subpath ]

                                    Err e ->
                                        text e
                           )

                SingleType IntegerType ->
                    val
                        |> Decode.decodeValue (Decode.at path Decode.int)
                        |> (\s ->
                                case s of
                                    Ok s ->
                                        Element.inputText None [ Attributes.type_ "number", onInput <| NumberChange jsonPointer ] (toString s)

                                    Err e ->
                                        text e
                           )

                SingleType NumberType ->
                    val
                        |> Decode.decodeValue (Decode.at path Decode.float)
                        |> (\s ->
                                case s of
                                    Ok s ->
                                        Element.inputText None [ Attributes.type_ "number", onInput <| NumberChange jsonPointer ] (toString s)

                                    Err e ->
                                        text e
                           )

                SingleType BooleanType ->
                    val
                        |> Decode.decodeValue (Decode.at path Decode.bool)
                        |> (\s ->
                                case s of
                                    Ok s ->
                                        Element.checkbox s None [ Attributes.type_ "checkbox", onCheck <| BooleanChange jsonPointer ] empty

                                    Err e ->
                                        text e
                           )

                SingleType ArrayType ->
                    val
                        |> Decode.decodeValue (Decode.at path (Decode.list Decode.value))
                        |> (\list ->
                                case list of
                                    Ok list ->
                                        text <| toString list

                                    Err e ->
                                        text e
                           )

                SingleType ObjectType ->
                    let
                        editAsValue =
                            False

                        v =
                            model.value |> Result.withDefault Encode.null
                    in
                        if editAsValue then
                            val
                                |> Encode.encode 4
                                |> Element.textArea JsonEditor [ Attributes.rows 10, onInput <| ValueChange jsonPointer ]
                        else
                            getFields v jsonPointer
                                |> Debug.log jsonPointer
                                |> List.map
                                    (\( name, _ ) ->
                                        row None
                                            []
                                            [ text <| "\"" ++ name ++ "\":"
                                              -- , Element.spacer 10
                                            , form model v schema subpath name
                                            ]
                                    )
                                |> column None [ inlineStyle [ ( "margin-left", "2ch" ) ] ]
                                |> (\x ->
                                        column None
                                            []
                                            [ text "{"
                                            , x
                                            , text "}"
                                            ]
                                   )

                x ->
                    x
                        |> toString
                        |> (++) "some other type detected: "
                        |> text
                        |> (\s -> [ s ])
                        |> column Error []
    in
        column None
            []
            [ controls
            ]
            |> Element.below
                [ model.valueUpdateErrors
                    |> Dict.get jsonPointer
                    |> Maybe.map (text >> (el InlineError []))
                    |> Maybe.withDefault empty
                ]


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
            Set.member subpath model.editPaths

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
                            "done editing"
                         else
                            "edit"
                        )
                ]

        schemaSource =
            s
                |> Schema.encode
                |> Encode.encode 2
                |> (\s -> "```json\n" ++ s ++ "```")
                |> Markdown.toHtml [ Html.Attributes.class "hljs" ]
                |> Element.html
                |> el SourceCode [ onDoubleClick <| ToggleEditing subpath ]

        editForm val =
            form model val model.schema subpath ""
                |> el SourceCode [ inlineStyle [ ( "margin", "5px" ) ] ]

        displaySchemaNode =
            if isEditMode then
                model.value
                    |> Result.withDefault Encode.null
                    |> editForm
            else
                schemaSource
    in
        displaySchemaNode
            |> Element.within [ toggleEditingButton ]


schemataKey : Int -> String -> String -> String -> View
schemataKey level parent s impliedType =
    let
        nodeName =
            if level == 0 then
                "h1"
            else
                "h2"
    in
        s
            |> text
            |> el SchemaHeader []
            |> Element.node nodeName


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
        dropAnchor newSubpath =
            el NoOutline
                [ Attributes.tabindex 1
                , Attributes.id <| String.dropLeft 1 newSubpath
                ]

        heading newSubpath key =
            newSubpath
                |> implied
                |> schemataKey level subpath key
                |> dropAnchor newSubpath

        implied : String -> String
        implied jsonPointer =
            ""

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
            let
                newSubpath =
                    subpath
                        ++ key
            in
                case metaSchema |> for subpath of
                    Just ms ->
                        if level == 0 then
                            [ key
                                |> heading newSubpath
                              --, displayNextToEachOther
                            , documentation
                                model
                                (level + 1)
                                newSubpath
                                schema
                                ms
                            ]
                        else
                            [ key
                                |> heading newSubpath
                              --, displayOneUnderAnother
                            , documentation
                                model
                                (level + 1)
                                newSubpath
                                schema
                                ms
                            ]

                    Nothing ->
                        []

        printSchemata (Schemata s) =
            col10
                [ s
                    |> List.map (printProperty >> col10)
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
