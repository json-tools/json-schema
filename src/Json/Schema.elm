module Json.Schema exposing (fromValue, fromString, validateValue)

{-|
This library provides bunch of utility methods to work with JSON values using
schemas defined in JSON Schema format.

Currently it allows to construct schemata (draft-6), validate values and generate random
values based on schema (very experimental feature).
It supports local references, but doesn't support remote references.

# Decode schema

Use `fromValue` or `fromString` methods if you receive schema from external source. If you want to construct schema from elm code you might want to use `Json.Schema.Builder`, or low-level API using definitions from `Json.Schema.Definitions`

@docs fromValue, fromString

# Validation

@docs validateValue

-}

import Json.Schema.Definitions exposing (Schema, decoder)
import Json.Schema.Validation exposing (Error, validate)
import Json.Decode exposing (Value, decodeValue, decodeString)


{-| Validate value against JSON Schema. Returns True Result if case if object is valid, or string error otherwise.
-}
validateValue : Value -> Schema -> Result (List Error) Value
validateValue =
    validate


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
