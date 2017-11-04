module Json.Schema.Random exposing (value, GeneratorSettings, defaultSettings)

import Json.Schema.Helpers exposing (resolve)
import Json.Schema.Definitions
    exposing
        ( Schema(ObjectSchema, BooleanSchema)
        , Schemata(Schemata)
        , Type(SingleType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType, NullType, ArrayType, ObjectType)
        , Items(ItemDefinition, ArrayOfItems, NoItems)
        )
import Json.Encode as Encode exposing (Value)
import Random exposing (Generator, Seed)
import Char
import Util exposing (getAt, uncons)


type alias GeneratorSettings =
    { optionalPropertyProbability : Float
    , degradationMultiplier : Float
    , defaultListLengthLimit : Int
    , defaultStringLengthLimit : Int
    }


defaultSettings : GeneratorSettings
defaultSettings =
    GeneratorSettings
        -- optionalPropertyProbability
        0.5
        -- degradationMultiplier
        0.2
        -- defaultListLengthLimit
        100
        -- defaultStringLengthLimit
        100


randomString : Int -> Int -> Maybe String -> Generator String
randomString minLength maxLength format =
    case format of
        Just "uri" ->
            Random.bool
                |> Random.map
                    (\x ->
                        if x then
                            "http://example.com/"
                        else
                            "https://github.com"
                    )

        Just "email" ->
            Random.int 1000 9999
                |> Random.map
                    (\x -> "rcp" ++ (toString x) ++ "@receipt.to")

        Just "host-name" ->
            Random.bool
                |> Random.map
                    (\x ->
                        if x then
                            "example.com"
                        else
                            "github.com"
                    )

        Just "date-time" ->
            Random.bool
                |> Random.map (\_ -> "2018-01-01T09:00:00Z")

        Just "time" ->
            Random.bool
                |> Random.map (\_ -> "09:00:00")

        Just "date" ->
            Random.bool
                |> Random.map (\_ -> "2018-01-01")

        _ ->
            Random.int minLength maxLength
                |> Random.andThen (flip Random.list lowercaseLetter)
                |> Random.map (String.fromList)


lowercaseLetter : Generator Char
lowercaseLetter =
    Random.map (\n -> Char.fromCode (n + 97)) (Random.int 0 25)


randomItemFromList : ( a, List a ) -> Generator a
randomItemFromList ( head, tail ) =
    let
        list =
            head :: tail
    in
        list
            |> List.length
            |> (+) -1
            |> Random.int 0
            |> Random.map (flip getAt list >> (Maybe.withDefault head))


nullGenerator : Generator Value
nullGenerator =
    Random.bool |> Random.map (\_ -> Encode.null)


upgradeSettings : GeneratorSettings -> GeneratorSettings
upgradeSettings settings =
    { settings
        | optionalPropertyProbability =
            settings.optionalPropertyProbability * settings.degradationMultiplier
    }


randomObject : GeneratorSettings -> Schema -> List ( String, Schema ) -> List String -> Generator Value
randomObject settings rootSchema props required =
    props
        |> List.foldl
            (\( k, v ) res ->
                if List.member k required then
                    v
                        |> valueGenerator (upgradeSettings settings) rootSchema
                        |> Random.andThen (\x -> res |> Random.map ((::) ( k, x )))
                else
                    Random.float 0 1
                        |> Random.andThen
                            (\isRequired ->
                                if isRequired < settings.optionalPropertyProbability then
                                    v
                                        |> valueGenerator (upgradeSettings settings) rootSchema
                                        |> Random.andThen (\x -> res |> Random.map ((::) ( k, x )))
                                else
                                    res
                            )
            )
            (Random.bool |> Random.map (\_ -> []))
        |> Random.map (List.reverse >> Encode.object)


randomList : GeneratorSettings -> Schema -> Int -> Int -> Schema -> Generator Value
randomList settings rootSchema minItems maxItems schema =
    Random.int minItems maxItems
        |> Random.andThen (flip Random.list (valueGenerator (upgradeSettings settings) rootSchema schema))
        |> Random.map (Encode.list)


value : GeneratorSettings -> Schema -> Generator Value
value settings s =
    valueGenerator settings s s


valueGenerator : GeneratorSettings -> Schema -> Schema -> Generator Value
valueGenerator settings rootSchema schema =
    case schema |> resolve rootSchema of
        BooleanSchema b ->
            if b then
                Random.bool |> Random.map (\_ -> Encode.object [])
            else
                Random.bool |> Random.map (\_ -> Encode.null)

        ObjectSchema os ->
            [ Maybe.andThen uncons os.examples
                |> Maybe.map randomItemFromList
            , Maybe.andThen uncons os.enum
                |> Maybe.map randomItemFromList
            , case os.type_ of
                SingleType NumberType ->
                    Random.float
                        (os.minimum |> Maybe.withDefault (toFloat Random.minInt))
                        (os.maximum |> Maybe.withDefault (toFloat Random.maxInt))
                        |> Random.map Encode.float
                        |> Just

                SingleType IntegerType ->
                    Random.int
                        (os.minimum |> Maybe.map round |> Maybe.withDefault Random.minInt)
                        (os.maximum |> Maybe.map round |> Maybe.withDefault Random.maxInt)
                        |> Random.map Encode.int
                        |> Just

                SingleType BooleanType ->
                    Random.bool
                        |> Random.map Encode.bool
                        |> Just

                SingleType StringType ->
                    randomString
                        (os.minLength |> Maybe.withDefault 0)
                        (os.maxLength |> Maybe.withDefault settings.defaultStringLengthLimit)
                        os.format
                        |> Random.map Encode.string
                        |> Just

                _ ->
                    Nothing
            , os.properties
                |> Maybe.map (\(Schemata props) -> randomObject settings rootSchema props (os.required |> Maybe.withDefault []))
            , case os.items of
                ItemDefinition schema ->
                    randomList settings
                        rootSchema
                        (os.minItems |> Maybe.withDefault 0)
                        (os.maxItems |> Maybe.withDefault settings.defaultListLengthLimit)
                        schema
                        |> Just

                --NoItems ->
                _ ->
                    Nothing
            ]
                |> List.foldl
                    (\maybeGenerator res ->
                        if res == Nothing then
                            maybeGenerator
                        else
                            res
                    )
                    Nothing
                |> Maybe.withDefault (nullGenerator)
