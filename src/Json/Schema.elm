module Json.Schema exposing
    ( fromValue, fromString
    , validateValue, validateAt
    )

{-| This library provides bunch of utility methods to work with JSON values using
schemas defined in [JSON Schema](http://json-schema.org/) format.

Currently it allows to construct schemata ([draft-6](https://github.com/json-schema-org/json-schema-spec/blob/draft-06/schema.json)), validate values and generate random
values based on schema (very experimental feature).
It supports local references, but doesn't support remote references.


# Decode schema

Use `fromValue` or `fromString` methods if you receive schema from external source. If you want to construct schema from elm code you might want to use `Json.Schema.Builder`, or low-level API using definitions from `Json.Schema.Definitions`

@docs fromValue, fromString


# Validation

@docs validateValue, validateAt

-}

import Json.Decode exposing (Value, decodeString, decodeValue)
import Json.Schema.Definitions exposing (Schema, decoder)
import Json.Schema.Helpers exposing (collectIds)
import Json.Schema.Validation exposing (Error, JsonPointer, ValidationError(..), ValidationOptions, validate)
import Json.Schemata
import Ref exposing (SchemataPool, defaultPool)


{-| Validate value against JSON Schema. Returns Result with updated value in case if validationOptions require so.

    schema
        |> Json.Schema.validateValue { applyDefaults = True } value

-}
validateValue : ValidationOptions -> Value -> Schema -> Result (List Error) Value
validateValue validationOptions value schema =
    let
        ( pool, _ ) =
            collectIds schema defaultPool
    in
    validate validationOptions pool value schema schema


{-| Validate value using subschema identified by URI.
-}
validateAt : ValidationOptions -> Value -> Schema -> String -> Result (List Error) Value
validateAt validationOptions value schema uri =
    let
        ( pool, _ ) =
            collectIds schema defaultPool
    in
    case Ref.resolveReference "" pool schema uri of
        Just ( ns, resolvedSchema ) ->
            validate validationOptions pool value schema resolvedSchema

        Nothing ->
            Err [ Error (JsonPointer "" []) <| UnresolvableReference uri ]


{-| Construct JSON Schema from JSON value
-}
fromValue : Value -> Result String Schema
fromValue =
    decodeValue decoder
        >> Result.mapError Json.Decode.errorToString


{-| Construct JSON Schema from string
-}
fromString : String -> Result String Schema
fromString =
    decodeString decoder
        >> Result.mapError Json.Decode.errorToString
