module Json.Schema exposing (fromValue, fromString, validateValue, validateAt)

{-|
This library provides bunch of utility methods to work with JSON values using
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

import Json.Schema.Definitions exposing (Schema, decoder)
import Ref exposing (defaultPool, SchemataPool)
import Json.Schemata
import Json.Schema.Validation exposing (Error, ValidationError(UnresolvableReference), JsonPointer, validate)
import Json.Decode exposing (Value, decodeValue, decodeString)
import Json.Schema.Helpers exposing (collectIds)


{-| Validate value against JSON Schema. Returns True Result if case if object is valid, or string error otherwise.
-}
validateValue : Value -> Schema -> Result (List Error) Value
validateValue value schema =
    validate (collectIds schema defaultPool) value schema schema


{-| Validate value using subschema identified by URI.
-}
validateAt : Value -> Schema -> String -> Result (List Error) Value
validateAt value schema uri =
    let
        pool =
            collectIds schema defaultPool
    in
        case Ref.resolveReference "" pool schema uri of
            Just ( ns, resolvedSchema ) ->
                validate pool value schema resolvedSchema

            Nothing ->
                Err [ Error (JsonPointer "" []) <| UnresolvableReference uri ]


{-| Construct JSON Schema from JSON value
-}
fromValue : Value -> Result String Schema
fromValue =
    decodeValue decoder


{-| Construct JSON Schema from string
-}
fromString : String -> Result String Schema
fromString =
    decodeString decoder
