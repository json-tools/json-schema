module Json.Schema.Examples exposing (coreSchemaDraft6, bookingSchema)

import Json.Schema.Builder exposing (..)
import Json.Encode as Encode
import Json.Decode
import Json.Schema.Definitions exposing (blankSchema, Schema)


coreSchemaDraft6 : Schema
coreSchemaDraft6 =
    buildSchema
        |> withUnionType [ "boolean", "object" ]
        |> withTitle "Core schema meta-schema"
        |> withDefault (Encode.object [])
        |> withDefinitions
            [ ( "schemaArray"
              , buildSchema
                    |> withType "array"
                    |> withItem (buildSchema |> withRef "#")
                    |> withMinItems 1
              )
            , ( "nonNegativeInteger"
              , buildSchema
                    |> withType "integer"
                    |> withMinimum 0
              )
            , ( "nonNegativeIntegerDefault0"
              , buildSchema
                    |> withAllOf
                        [ buildSchema
                            |> withRef "#/definitions/nonNegativeInteger"
                        , buildSchema
                            |> withDefault (Encode.int 0)
                        ]
              )
            , ( "simpleTypes"
              , buildSchema
                    |> withEnum
                        ([ "array"
                         , "boolean"
                         , "integer"
                         , "null"
                         , "number"
                         , "object"
                         , "string"
                         ]
                            |> List.map Encode.string
                        )
              )
            , ( "stringArray"
              , buildSchema
                    |> withType "array"
                    |> withDefault ([] |> Encode.list)
                    |> withItem (buildSchema |> withType "string")
              )
            ]
        |> withProperties
            [ ( "$id"
              , buildSchema
                    |> withType "string"
                    |> withFormat "uri-reference"
              )
            , ( "$schema"
              , buildSchema
                    |> withType "string"
                    |> withFormat "uri"
              )
            , ( "$ref"
              , buildSchema
                    |> withType "string"
                    |> withFormat "uri-reference"
              )
            , ( "title"
              , buildSchema |> withType "string"
              )
            , ( "description"
              , buildSchema |> withType "string"
              )
            , ( "default"
              , buildSchema
              )
            , ( "multipleOf"
              , buildSchema
                    |> withType "number"
                    |> withExclusiveMinimum 0
              )
            , ( "maximum"
              , buildSchema |> withType "number"
              )
            , ( "exclusiveMaximum"
              , buildSchema |> withType "number"
              )
            , ( "minimum"
              , buildSchema |> withType "number"
              )
            , ( "exclusiveMinimum"
              , buildSchema |> withType "number"
              )
            , ( "maxLength"
              , buildSchema |> withRef "#/definitions/nonNegativeInteger"
              )
            , ( "minLength"
              , buildSchema |> withRef "#/definitions/nonNegativeIntegerDefault0"
              )
            , ( "pattern"
              , buildSchema
                    |> withType "string"
                    |> withFormat "regex"
              )
            , ( "additionalItems"
              , buildSchema |> withRef "#"
              )
            , ( "items"
              , buildSchema
                    |> withDefault (Encode.object [])
                    |> withAnyOf
                        [ buildSchema |> withRef "#"
                        , buildSchema |> withRef "#/definitions/schemaArray"
                        ]
              )
            , ( "maxItems"
              , buildSchema |> withRef "#/definitions/nonNegativeInteger"
              )
            , ( "minItems"
              , buildSchema |> withRef "#/definitions/nonNegativeIntegerDefault0"
              )
            , ( "uniqueItems"
              , buildSchema
                    |> withType "boolean"
                    |> withDefault (Encode.bool False)
              )
            , ( "contains"
              , buildSchema |> withRef "#"
              )
            , ( "maxProperties"
              , buildSchema |> withRef "#/definitions/nonNegativeInteger"
              )
            , ( "minProperties"
              , buildSchema |> withRef "#/definitions/nonNegativeIntegerDefault0"
              )
            , ( "required"
              , buildSchema |> withRef "#/definitions/stringArray"
              )
            , ( "additionalProperties"
              , buildSchema |> withRef "#"
              )
            , ( "definitions"
              , buildSchema
                    |> withType "object"
                    |> withDefault (Encode.object [])
                    |> withAdditionalProperties (buildSchema |> withRef "#")
              )
            , ( "properties"
              , buildSchema
                    |> withType "object"
                    |> withDefault (Encode.object [])
                    |> withAdditionalProperties (buildSchema |> withRef "#")
              )
            , ( "patternProperties"
              , buildSchema
                    |> withType "object"
                    |> withDefault (Encode.object [])
                    |> withAdditionalProperties (buildSchema |> withRef "#")
                -- |> withPropertyNames (buildSchema |> withFormat "regex")
              )
            , ( "dependencies"
              , buildSchema
                    |> withType "object"
                    |> withAdditionalProperties
                        (buildSchema
                            |> withAnyOf
                                [ buildSchema |> withRef "#"
                                , buildSchema |> withRef "#/definitions/stringArray"
                                ]
                        )
              )
            , ( "propertyNames"
              , buildSchema |> withRef "#"
              )
            , ( "const"
              , buildSchema
              )
            , ( "enum"
              , buildSchema
                    |> withType "array"
                    |> withMinItems 1
                    |> withUniqueItems True
              )
            , ( "type"
              , buildSchema
                    |> withAnyOf
                        [ buildSchema
                            |> withRef "#/definitions/simpleTypes"
                        , buildSchema
                            |> withType "array"
                            |> withItem (buildSchema |> withRef "#/definitions/simpleTypes")
                            |> withMinItems 1
                            |> withUniqueItems True
                        ]
              )
            , ( "format"
              , buildSchema |> withType "string"
              )
            , ( "allOf"
              , buildSchema |> withRef "#/definitions/schemaArray"
              )
            , ( "anyOf"
              , buildSchema |> withRef "#/definitions/schemaArray"
              )
            , ( "oneOf"
              , buildSchema |> withRef "#/definitions/schemaArray"
              )
            , ( "not"
              , buildSchema |> withRef "#"
              )
            ]
        |> toSchema
        |> Result.withDefault blankSchema


bookingSchema : Schema
bookingSchema =
    buildSchema
        |> withType "object"
        |> withDefinitions
            [ ( "basicPerson"
              , buildSchema
                    |> withType "object"
                    |> withTitle "Basic Person"
                    |> withDescription "Minimal information representing a person"
                    |> withRequired [ "title", "firstName", "lastName" ]
                    |> withProperties
                        [ ( "title"
                          , buildSchema
                                |> withEnum ([ "mr", "miss", "ms", "mrs" ] |> List.map Encode.string)
                          )
                        , ( "firstName"
                          , buildSchema
                                |> withType "string"
                          )
                        , ( "lastName"
                          , buildSchema
                                |> withType "string"
                          )
                        ]
              )
            , ( "personAddress"
              , buildSchema
                    |> withTitle "Person+Address"
                    |> withDescription "Weird combination of a person with an address, early days [Selective breeding](https://en.wikipedia.org/wiki/Selective_breeding) experiment."
                    |> withAllOf
                        [ buildSchema
                            |> withRef "#/definitions/basicPerson"
                        , buildSchema
                            |> withRef "#/definitions/address"
                        ]
              )
            , ( "phone"
              , buildSchema
                    |> withType "object"
                    |> withTitle "Phone number"
                    |> withRequired [ "countryCode", "number" ]
                    |> withProperties
                        [ ( "countryCode"
                          , buildSchema
                                |> withRef "#/definitions/countryCode"
                          )
                        , ( "number"
                          , buildSchema
                                |> withType "string"
                                |> withDescription "Mobile phone number (numbers only, excluding country code)"
                                |> withMinLength 10
                          )
                        ]
              )
            , ( "price"
              , buildSchema
                    |> withType "object"
                    |> withRequired [ "currencyCode", "value" ]
                    |> withProperties
                        [ ( "currencyCode"
                          , buildSchema
                                |> withRef "#/definitions/currencyCode"
                          )
                        , ( "value"
                          , buildSchema
                                |> withType "integer"
                                |> withDescription "A positive integer in the smallest currency unit (that is, 100 pence for £1.00)"
                                |> withMinimum 0
                          )
                        ]
                    |> withAdditionalProperties (boolSchema False)
              )
            , ( "paymentCard"
              , buildSchema
                    |> withType "object"
                    |> withTitle "Payment Card"
                    |> withDescription "Note that instead of card number `panToken` must be supplied because of PCI DSS Compliance limitations"
                    |> withRequired [ "type", "brand", "expirationDate", "name", "cvv" ]
                    |> withProperties
                        [ ( "type"
                          , buildSchema
                                |> withType "string"
                                |> withEnum ([ "debit", "credit" ] |> List.map Encode.string)
                          )
                        , ( "brand"
                          , buildSchema
                                |> withType "string"
                                |> withEnum ([ "visa", "mastercard", "amex" ] |> List.map Encode.string)
                          )
                        , ( "panToken"
                          , buildSchema
                                |> withType "string"
                                |> withMinLength 20
                          )
                        , ( "expirationDate"
                          , buildSchema
                                |> withType "string"
                                |> withMaxLength 7
                                |> withMinLength 7
                                |> withPattern "^20[0-9]{2}-(?:0[1-9]|1[0-2])$"
                          )
                        , ( "name"
                          , buildSchema
                                |> withType "string"
                          )
                        , ( "cvv"
                          , buildSchema
                                |> withType "string"
                                |> withMaxLength 4
                                |> withMinLength 3
                          )
                        ]
              )
            , ( "address"
              , buildSchema
                    |> withType "object"
                    |> withRequired [ "line1", "city", "postcode", "countryCode" ]
                    |> withProperties
                        [ ( "line1"
                          , buildSchema
                                |> withType "string"
                                |> withTitle "Address line 1"
                                |> withDescription "Street name with house number"
                          )
                        , ( "line2"
                          , buildSchema
                                |> withType "string"
                                |> withTitle "Address line 2"
                                |> withDescription "Additional address info"
                          )
                        , ( "city"
                          , buildSchema
                                |> withType "string"
                          )
                        , ( "postcode"
                          , buildSchema
                                |> withType "string"
                          )
                        , ( "countryCode"
                          , buildSchema
                                |> withRef "#/definitions/countryCode"
                          )
                        ]
              )
            , ( "datePlace"
              , buildSchema
                    |> withType "object"
                    |> withRequired [ "dateTime", "airportCode" ]
                    |> withProperties
                        [ ( "countryCode"
                          , buildSchema
                                |> withRef "#/definitions/countryCode"
                          )
                        , ( "dateTime"
                          , buildSchema
                                |> withType "string"
                                |> withTitle "Date-Time"
                                |> withDescription "Date and time of flight (airport local time)"
                                |> withPattern "^20[0-9]{2}-(?:0[1-9]|1[0-2])-(?:0[1-9]|[1-3][0-9]) [012][0-9]:[0-5][0-9]$"
                          )
                        , ( "airportCode"
                          , buildSchema
                                |> withType "string"
                                |> withTitle "Airport Code"
                                |> withDescription "International Air Transport Association airport code"
                                |> withMaxLength 3
                                |> withMinLength 3
                                |> withPattern "^[A-Z]{3}$"
                          )
                        ]
                    |> withPropertyNames buildSchema
                    |> withEnum ([ "dateTime", "airportCode", "countryCode" ] |> List.map Encode.string)
              )
            , ( "currencyCode"
              , buildSchema
                    |> withType "string"
                    |> withDescription "3-letter ISO code representing the currency. __Lowercase__."
                    |> withMaxLength 3
                    |> withMinLength 3
                    |> withEnum ([ "all", "afn", "ars", "awg", "aud", "azn", "bsd", "bbd", "byn", "bzd", "bmd", "bob", "bam", "bwp", "bgn", "brl", "bnd", "khr", "cad", "kyd", "clp", "cny", "cop", "crc", "hrk", "cup", "czk", "dkk", "dop", "xcd", "egp", "svc", "eur", "fkp", "fjd", "ghs", "gip", "gtq", "ggp", "gyd", "hnl", "hkd", "huf", "isk", "inr", "idr", "irr", "imp", "ils", "jmd", "jpy", "jep", "kzt", "kpw", "krw", "kgs", "lak", "lbp", "lrd", "mkd", "myr", "mur", "mxn", "mnt", "mzn", "nad", "npr", "ang", "nzd", "nio", "ngn", "nok", "omr", "pkr", "pab", "pyg", "pen", "php", "pln", "qar", "ron", "rub", "shp", "sar", "rsd", "scr", "sgd", "sbd", "sos", "zar", "lkr", "sek", "chf", "srd", "syp", "twd", "thb", "ttd", "try", "tvd", "uah", "gbp", "usd", "uyu", "uzs", "vef", "vnd", "yer", "zwd" ] |> List.map Encode.string)
              )
            , ( "countryCode"
              , buildSchema
                    |> withType "string"
                    |> withTitle "ISO code representing the country"
                    |> withDescription "2-letter ISO code representing the country. United Kingdom is officially assigned the alpha-2 code `gb` rather than `uk`. __Lowercase__."
                    |> withMaxLength 2
                    |> withMinLength 2
                    |> withEnum ([ "af", "ax", "al", "dz", "as", "ad", "ao", "ai", "aq", "ag", "ar", "am", "aw", "au", "at", "az", "bs", "bh", "bd", "bb", "by", "be", "bz", "bj", "bm", "bt", "bo", "bq", "ba", "bw", "bv", "br", "io", "bn", "bg", "bf", "bi", "kh", "cm", "ca", "cv", "ky", "cf", "td", "cl", "cn", "cx", "cc", "co", "km", "cg", "cd", "ck", "cr", "ci", "hr", "cu", "cw", "cy", "cz", "dk", "dj", "dm", "do", "ec", "eg", "sv", "gq", "er", "ee", "et", "fk", "fo", "fj", "fi", "fr", "gf", "pf", "tf", "ga", "gm", "ge", "de", "gh", "gi", "gr", "gl", "gd", "gp", "gu", "gt", "gg", "gn", "gw", "gy", "ht", "hm", "va", "hn", "hk", "hu", "is", "in", "id", "ir", "iq", "ie", "im", "il", "it", "jm", "jp", "je", "jo", "kz", "ke", "ki", "kp", "kr", "kw", "kg", "la", "lv", "lb", "ls", "lr", "ly", "li", "lt", "lu", "mo", "mk", "mg", "mw", "my", "mv", "ml", "mt", "mh", "mq", "mr", "mu", "yt", "mx", "fm", "md", "mc", "mn", "me", "ms", "ma", "mz", "mm", "na", "nr", "np", "nl", "nc", "nz", "ni", "ne", "ng", "nu", "nf", "mp", "no", "om", "pk", "pw", "ps", "pa", "pg", "py", "pe", "ph", "pn", "pl", "pt", "pr", "qa", "re", "ro", "ru", "rw", "bl", "sh", "kn", "lc", "mf", "pm", "vc", "ws", "sm", "st", "sa", "sn", "rs", "sc", "sl", "sg", "sx", "sk", "si", "sb", "so", "za", "gs", "ss", "es", "lk", "sd", "sr", "sj", "sz", "se", "ch", "sy", "tw", "tj", "tz", "th", "tl", "tg", "tk", "to", "tt", "tn", "tr", "tm", "tc", "tv", "ug", "ua", "ae", "gb", "us", "um", "uy", "uz", "vu", "ve", "vn", "vg", "vi", "wf", "eh", "ye", "zm", "zw" ] |> List.map Encode.string)
              )
            ]
        |> withRequired [ "url", "account", "passengers", "payment", "flight" ]
        |> withProperties
            [ ( "url"
              , buildSchema
                    |> withType "string"
                    |> withFormat "uri"
              )
            , ( "account"
              , buildSchema
                    |> withType "object"
                    |> withRequired [ "email", "phone", "isExisting" ]
                    |> withProperties
                        [ ( "email"
                          , buildSchema
                                |> withType "string"
                                |> withFormat "email"
                          )
                        , ( "password"
                          , buildSchema
                                |> withType "string"
                                |> withFormat "string"
                          )
                        , ( "phone"
                          , buildSchema
                                |> withRef "#/definitions/phone"
                          )
                        , ( "isExisting"
                          , buildSchema
                                |> withType "boolean"
                                |> withEnum ([ True, False ] |> List.map Encode.bool)
                          )
                        ]
              )
            , ( "flight"
              , buildSchema
                    |> withType "object"
                    |> withRequired [ "cabinClass", "from", "to", "price" ]
                    |> withProperties
                        [ ( "from"
                          , buildSchema
                                |> withRef "#/definitions/datePlace"
                          )
                        , ( "to"
                          , buildSchema
                                |> withRef "#/definitions/datePlace"
                          )
                        , ( "return"
                          , buildSchema
                                |> withType "object"
                                |> withRequired [ "from", "to" ]
                                |> withProperties
                                    [ ( "from"
                                      , buildSchema
                                            |> withRef "#/definitions/datePlace"
                                      )
                                    , ( "to"
                                      , buildSchema
                                            |> withRef "#/definitions/datePlace"
                                      )
                                    ]
                          )
                        , ( "price"
                          , buildSchema
                                |> withRef "#/definitions/price"
                          )
                        , ( "cabinClass"
                          , buildSchema
                                |> withType "string"
                                |> withEnum ([ "economy", "economy premium", "business", "first" ] |> List.map Encode.string)
                          )
                        ]
                    |> withAdditionalProperties (boolSchema True)
              )
            , ( "passengers"
              , buildSchema
                    |> withType "array"
                    |> withItem
                        (buildSchema
                            |> withType "object"
                            |> withRequired [ "title", "firstName", "lastName", "dateOfBirth", "hasHoldLuggage" ]
                            |> withProperties
                                [ ( "title"
                                  , buildSchema
                                        |> withEnum ([ "mr", "miss", "ms", "mrs" ] |> List.map Encode.string)
                                  )
                                , ( "firstName"
                                  , buildSchema
                                        |> withType "string"
                                  )
                                , ( "lastName"
                                  , buildSchema
                                        |> withType "string"
                                  )
                                , ( "dateOfBirth"
                                  , buildSchema
                                        |> withType "string"
                                        |> withFormat "date"
                                  )
                                , ( "hasHoldLuggage"
                                  , buildSchema
                                        |> withType "boolean"
                                        |> withEnum ([ True, False ] |> List.map Encode.bool)
                                  )
                                , ( "id"
                                  , buildSchema
                                        |> withType "object"
                                        |> withProperties
                                            [ ( "type"
                                              , buildSchema
                                                    |> withType "string"
                                              )
                                            , ( "number"
                                              , buildSchema
                                                    |> withType "string"
                                              )
                                            , ( "expDate"
                                              , buildSchema
                                                    |> withType "string"
                                                    |> withFormat "date"
                                              )
                                            , ( "countryCode"
                                              , buildSchema
                                                    |> withRef "#/definitions/countryCode"
                                              )
                                            ]
                                  )
                                ]
                        )
                    |> withMaxItems 1
                    |> withMinItems 1
              )
            , ( "payment"
              , buildSchema
                    |> withType "object"
                    |> withRequired [ "card", "address" ]
                    |> withProperties
                        [ ( "card"
                          , buildSchema
                                |> withRef "#/definitions/paymentCard"
                          )
                        , ( "address"
                          , buildSchema
                                |> withRef "#/definitions/personAddress"
                          )
                        ]
              )
            ]
        |> withAdditionalProperties (boolSchema False)
        |> toSchema
        |> Result.withDefault blankSchema
