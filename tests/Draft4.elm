module Draft4 exposing (all)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (decodeString, value)
import Json.Schema.Definitions exposing (blankSchema, decoder)
import Json.Schema exposing (validateValue)
import Test exposing (Test, describe, test, only)
import Expect


all : Test
all =
    describe "draft-4"
        [ describe "additionalItems.json"
            [ describe "suite: additionalItems as schema"
                [ test "additional items match schema" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {}
                                ],
                                "additionalItems": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            [
                                null,
                                2,
                                3,
                                4
                            ]
                            """
                            True
                , test "additional items do not match schema" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {}
                                ],
                                "additionalItems": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            [
                                null,
                                2,
                                3,
                                "foo"
                            ]
                            """
                            False
                ]
            , describe "suite: items is schema, no additionalItems"
                [ test "all items match schema" <|
                    \() ->
                        examine
                            """
                            {
                                "items": {},
                                "additionalItems": false
                            }
                            """
                            """
                            [
                                1,
                                2,
                                3,
                                4,
                                5
                            ]
                            """
                            True
                ]
            , describe "suite: array of items with no additionalItems"
                [ test "fewer number of items present" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {},
                                    {},
                                    {}
                                ],
                                "additionalItems": false
                            }
                            """
                            """
                            [
                                1,
                                2
                            ]
                            """
                            True
                , test "equal number of items present" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {},
                                    {},
                                    {}
                                ],
                                "additionalItems": false
                            }
                            """
                            """
                            [
                                1,
                                2,
                                3
                            ]
                            """
                            True
                , test "additional items are not permitted" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {},
                                    {},
                                    {}
                                ],
                                "additionalItems": false
                            }
                            """
                            """
                            [
                                1,
                                2,
                                3,
                                4
                            ]
                            """
                            False
                ]
            , describe "suite: additionalItems as false without items"
                [ test "items defaults to empty schema so everything is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "additionalItems": false
                            }
                            """
                            """
                            [
                                1,
                                2,
                                3,
                                4,
                                5
                            ]
                            """
                            True
                , test "ignores non-arrays" <|
                    \() ->
                        examine
                            """
                            {
                                "additionalItems": false
                            }
                            """
                            """
                            {
                                "foo": "bar"
                            }
                            """
                            True
                ]
            , describe "suite: additionalItems are allowed by default"
                [ test "only the first item is validated" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {
                                        "type": "integer"
                                    }
                                ]
                            }
                            """
                            """
                            [
                                1,
                                "foo",
                                false
                            ]
                            """
                            True
                ]
            ]
        , describe "additionalProperties.json"
            [ describe "suite: additionalProperties being false does not allow other properties"
                [ test "no additional properties is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "patternProperties": {
                                    "^v": {}
                                },
                                "additionalProperties": false
                            }
                            """
                            """
                            {
                                "foo": 1
                            }
                            """
                            True
                , test "an additional property is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "patternProperties": {
                                    "^v": {}
                                },
                                "additionalProperties": false
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2,
                                "quux": "boom"
                            }
                            """
                            False
                , test "ignores arrays" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "patternProperties": {
                                    "^v": {}
                                },
                                "additionalProperties": false
                            }
                            """
                            """
                            [
                                1,
                                2,
                                3
                            ]
                            """
                            True
                , test "ignores strings" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "patternProperties": {
                                    "^v": {}
                                },
                                "additionalProperties": false
                            }
                            """
                            """
                            "foobarbaz"
                            """
                            True
                , test "ignores other non-objects" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "patternProperties": {
                                    "^v": {}
                                },
                                "additionalProperties": false
                            }
                            """
                            """
                            12
                            """
                            True
                , test "patternProperties are not additional properties" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "patternProperties": {
                                    "^v": {}
                                },
                                "additionalProperties": false
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "vroom": 2
                            }
                            """
                            True
                ]
            , describe "suite: additionalProperties allows a schema which should validate"
                [ test "no additional properties is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "additionalProperties": {
                                    "type": "boolean"
                                }
                            }
                            """
                            """
                            {
                                "foo": 1
                            }
                            """
                            True
                , test "an additional valid property is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "additionalProperties": {
                                    "type": "boolean"
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2,
                                "quux": true
                            }
                            """
                            True
                , test "an additional invalid property is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "additionalProperties": {
                                    "type": "boolean"
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2,
                                "quux": 12
                            }
                            """
                            False
                ]
            , describe "suite: additionalProperties can exist by itself"
                [ test "an additional valid property is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "additionalProperties": {
                                    "type": "boolean"
                                }
                            }
                            """
                            """
                            {
                                "foo": true
                            }
                            """
                            True
                , test "an additional invalid property is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "additionalProperties": {
                                    "type": "boolean"
                                }
                            }
                            """
                            """
                            {
                                "foo": 1
                            }
                            """
                            False
                ]
            , describe "suite: additionalProperties are allowed by default"
                [ test "additional properties are allowed" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2,
                                "quux": true
                            }
                            """
                            True
                ]
            ]
        , describe "allOf.json"
            [ describe "suite: allOf"
                [ test "allOf" <|
                    \() ->
                        examine
                            """
                            {
                                "allOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": "baz",
                                "bar": 2
                            }
                            """
                            True
                , test "mismatch second" <|
                    \() ->
                        examine
                            """
                            {
                                "allOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": "baz"
                            }
                            """
                            False
                , test "mismatch first" <|
                    \() ->
                        examine
                            """
                            {
                                "allOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "bar": 2
                            }
                            """
                            False
                , test "wrong type" <|
                    \() ->
                        examine
                            """
                            {
                                "allOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": "baz",
                                "bar": "quux"
                            }
                            """
                            False
                ]
            , describe "suite: allOf with base schema"
                [ test "valid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "bar": {
                                        "type": "integer"
                                    }
                                },
                                "required": [
                                    "bar"
                                ],
                                "allOf": [
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "baz": {
                                                "type": "null"
                                            }
                                        },
                                        "required": [
                                            "baz"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": "quux",
                                "bar": 2,
                                "baz": null
                            }
                            """
                            True
                , test "mismatch base schema" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "bar": {
                                        "type": "integer"
                                    }
                                },
                                "required": [
                                    "bar"
                                ],
                                "allOf": [
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "baz": {
                                                "type": "null"
                                            }
                                        },
                                        "required": [
                                            "baz"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": "quux",
                                "baz": null
                            }
                            """
                            False
                , test "mismatch first allOf" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "bar": {
                                        "type": "integer"
                                    }
                                },
                                "required": [
                                    "bar"
                                ],
                                "allOf": [
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "baz": {
                                                "type": "null"
                                            }
                                        },
                                        "required": [
                                            "baz"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "bar": 2,
                                "baz": null
                            }
                            """
                            False
                , test "mismatch second allOf" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "bar": {
                                        "type": "integer"
                                    }
                                },
                                "required": [
                                    "bar"
                                ],
                                "allOf": [
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "baz": {
                                                "type": "null"
                                            }
                                        },
                                        "required": [
                                            "baz"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": "quux",
                                "bar": 2
                            }
                            """
                            False
                , test "mismatch both" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "bar": {
                                        "type": "integer"
                                    }
                                },
                                "required": [
                                    "bar"
                                ],
                                "allOf": [
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "baz": {
                                                "type": "null"
                                            }
                                        },
                                        "required": [
                                            "baz"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "bar": 2
                            }
                            """
                            False
                ]
            , describe "suite: allOf simple types"
                [ test "valid" <|
                    \() ->
                        examine
                            """
                            {
                                "allOf": [
                                    {
                                        "maximum": 30
                                    },
                                    {
                                        "minimum": 20
                                    }
                                ]
                            }
                            """
                            """
                            25
                            """
                            True
                , test "mismatch one" <|
                    \() ->
                        examine
                            """
                            {
                                "allOf": [
                                    {
                                        "maximum": 30
                                    },
                                    {
                                        "minimum": 20
                                    }
                                ]
                            }
                            """
                            """
                            35
                            """
                            False
                ]
            ]
        , describe "anyOf.json"
            [ describe "suite: anyOf"
                [ test "first anyOf valid" <|
                    \() ->
                        examine
                            """
                            {
                                "anyOf": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "minimum": 2
                                    }
                                ]
                            }
                            """
                            """
                            1
                            """
                            True
                , test "second anyOf valid" <|
                    \() ->
                        examine
                            """
                            {
                                "anyOf": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "minimum": 2
                                    }
                                ]
                            }
                            """
                            """
                            2.5
                            """
                            True
                , test "both anyOf valid" <|
                    \() ->
                        examine
                            """
                            {
                                "anyOf": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "minimum": 2
                                    }
                                ]
                            }
                            """
                            """
                            3
                            """
                            True
                , test "neither anyOf valid" <|
                    \() ->
                        examine
                            """
                            {
                                "anyOf": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "minimum": 2
                                    }
                                ]
                            }
                            """
                            """
                            1.5
                            """
                            False
                ]
            , describe "suite: anyOf with base schema"
                [ test "mismatch base schema" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string",
                                "anyOf": [
                                    {
                                        "maxLength": 2
                                    },
                                    {
                                        "minLength": 4
                                    }
                                ]
                            }
                            """
                            """
                            3
                            """
                            False
                , test "one anyOf valid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string",
                                "anyOf": [
                                    {
                                        "maxLength": 2
                                    },
                                    {
                                        "minLength": 4
                                    }
                                ]
                            }
                            """
                            """
                            "foobar"
                            """
                            True
                , test "both anyOf invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string",
                                "anyOf": [
                                    {
                                        "maxLength": 2
                                    },
                                    {
                                        "minLength": 4
                                    }
                                ]
                            }
                            """
                            """
                            "foo"
                            """
                            False
                ]
            , describe "suite: anyOf complex types"
                [ test "first anyOf valid (complex)" <|
                    \() ->
                        examine
                            """
                            {
                                "anyOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "bar": 2
                            }
                            """
                            True
                , test "second anyOf valid (complex)" <|
                    \() ->
                        examine
                            """
                            {
                                "anyOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": "baz"
                            }
                            """
                            True
                , test "both anyOf valid (complex)" <|
                    \() ->
                        examine
                            """
                            {
                                "anyOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": "baz",
                                "bar": 2
                            }
                            """
                            True
                , test "neither anyOf valid (complex)" <|
                    \() ->
                        examine
                            """
                            {
                                "anyOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": 2,
                                "bar": "quux"
                            }
                            """
                            False
                ]
            ]
        , describe "default.json"
            [ describe "suite: invalid type for default"
                [ test "valid when property is specified" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "integer",
                                        "default": []
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": 13
                            }
                            """
                            True
                , test "still valid when the invalid default is used" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "integer",
                                        "default": []
                                    }
                                }
                            }
                            """
                            """
                            {}
                            """
                            True
                ]
            , describe "suite: invalid string value for default"
                [ test "valid when property is specified" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "bar": {
                                        "type": "string",
                                        "minLength": 4,
                                        "default": "bad"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "bar": "good"
                            }
                            """
                            True
                , test "still valid when the invalid default is used" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "bar": {
                                        "type": "string",
                                        "minLength": 4,
                                        "default": "bad"
                                    }
                                }
                            }
                            """
                            """
                            {}
                            """
                            True
                ]
            ]
        , describe "definitions.json"
            [ describe "suite: valid definition"
                [ test "valid definition schema" <|
                    \() ->
                        examine
                            """
                            {
                                "$ref": "http://json-schema.org/draft-04/schema#"
                            }
                            """
                            """
                            {
                                "definitions": {
                                    "foo": {
                                        "type": "integer"
                                    }
                                }
                            }
                            """
                            True
                ]
            , describe "suite: invalid definition"
                [ test "invalid definition schema" <|
                    \() ->
                        examine
                            """
                            {
                                "$ref": "http://json-schema.org/draft-04/schema#"
                            }
                            """
                            """
                            {
                                "definitions": {
                                    "foo": {
                                        "type": 1
                                    }
                                }
                            }
                            """
                            False
                ]
            ]
        , describe "dependencies.json"
            [ describe "suite: dependencies"
                [ test "neither" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": [
                                        "foo"
                                    ]
                                }
                            }
                            """
                            """
                            {}
                            """
                            True
                , test "nondependant" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": [
                                        "foo"
                                    ]
                                }
                            }
                            """
                            """
                            {
                                "foo": 1
                            }
                            """
                            True
                , test "with dependency" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": [
                                        "foo"
                                    ]
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2
                            }
                            """
                            True
                , test "missing dependency" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": [
                                        "foo"
                                    ]
                                }
                            }
                            """
                            """
                            {
                                "bar": 2
                            }
                            """
                            False
                , test "ignores arrays" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": [
                                        "foo"
                                    ]
                                }
                            }
                            """
                            """
                            [
                                "bar"
                            ]
                            """
                            True
                , test "ignores strings" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": [
                                        "foo"
                                    ]
                                }
                            }
                            """
                            """
                            "foobar"
                            """
                            True
                , test "ignores other non-objects" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": [
                                        "foo"
                                    ]
                                }
                            }
                            """
                            """
                            12
                            """
                            True
                ]
            , describe "suite: multiple dependencies"
                [ test "neither" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "quux": [
                                        "foo",
                                        "bar"
                                    ]
                                }
                            }
                            """
                            """
                            {}
                            """
                            True
                , test "nondependants" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "quux": [
                                        "foo",
                                        "bar"
                                    ]
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2
                            }
                            """
                            True
                , test "with dependencies" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "quux": [
                                        "foo",
                                        "bar"
                                    ]
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2,
                                "quux": 3
                            }
                            """
                            True
                , test "missing dependency" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "quux": [
                                        "foo",
                                        "bar"
                                    ]
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "quux": 2
                            }
                            """
                            False
                , test "missing other dependency" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "quux": [
                                        "foo",
                                        "bar"
                                    ]
                                }
                            }
                            """
                            """
                            {
                                "bar": 1,
                                "quux": 2
                            }
                            """
                            False
                , test "missing both dependencies" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "quux": [
                                        "foo",
                                        "bar"
                                    ]
                                }
                            }
                            """
                            """
                            {
                                "quux": 1
                            }
                            """
                            False
                ]
            , describe "suite: multiple dependencies subschema"
                [ test "valid" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": {
                                        "properties": {
                                            "foo": {
                                                "type": "integer"
                                            },
                                            "bar": {
                                                "type": "integer"
                                            }
                                        }
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2
                            }
                            """
                            True
                , test "no dependency" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": {
                                        "properties": {
                                            "foo": {
                                                "type": "integer"
                                            },
                                            "bar": {
                                                "type": "integer"
                                            }
                                        }
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": "quux"
                            }
                            """
                            True
                , test "wrong type" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": {
                                        "properties": {
                                            "foo": {
                                                "type": "integer"
                                            },
                                            "bar": {
                                                "type": "integer"
                                            }
                                        }
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": "quux",
                                "bar": 2
                            }
                            """
                            False
                , test "wrong type other" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": {
                                        "properties": {
                                            "foo": {
                                                "type": "integer"
                                            },
                                            "bar": {
                                                "type": "integer"
                                            }
                                        }
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": 2,
                                "bar": "quux"
                            }
                            """
                            False
                , test "wrong type both" <|
                    \() ->
                        examine
                            """
                            {
                                "dependencies": {
                                    "bar": {
                                        "properties": {
                                            "foo": {
                                                "type": "integer"
                                            },
                                            "bar": {
                                                "type": "integer"
                                            }
                                        }
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": "quux",
                                "bar": "quux"
                            }
                            """
                            False
                ]
            ]
        , describe "enum.json"
            [ describe "suite: simple enum validation"
                [ test "one of the enum is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "enum": [
                                    1,
                                    2,
                                    3
                                ]
                            }
                            """
                            """
                            1
                            """
                            True
                , test "something else is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "enum": [
                                    1,
                                    2,
                                    3
                                ]
                            }
                            """
                            """
                            4
                            """
                            False
                ]
            , describe "suite: heterogeneous enum validation"
                [ test "one of the enum is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "enum": [
                                    6,
                                    "foo",
                                    [],
                                    true,
                                    {
                                        "foo": 12
                                    }
                                ]
                            }
                            """
                            """
                            []
                            """
                            True
                , test "something else is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "enum": [
                                    6,
                                    "foo",
                                    [],
                                    true,
                                    {
                                        "foo": 12
                                    }
                                ]
                            }
                            """
                            """
                            null
                            """
                            False
                , test "objects are deep compared" <|
                    \() ->
                        examine
                            """
                            {
                                "enum": [
                                    6,
                                    "foo",
                                    [],
                                    true,
                                    {
                                        "foo": 12
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": false
                            }
                            """
                            False
                ]
            , describe "suite: enums in properties"
                [ test "both properties are valid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "object",
                                "properties": {
                                    "foo": {
                                        "enum": [
                                            "foo"
                                        ]
                                    },
                                    "bar": {
                                        "enum": [
                                            "bar"
                                        ]
                                    }
                                },
                                "required": [
                                    "bar"
                                ]
                            }
                            """
                            """
                            {
                                "foo": "foo",
                                "bar": "bar"
                            }
                            """
                            True
                , test "missing optional property is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "object",
                                "properties": {
                                    "foo": {
                                        "enum": [
                                            "foo"
                                        ]
                                    },
                                    "bar": {
                                        "enum": [
                                            "bar"
                                        ]
                                    }
                                },
                                "required": [
                                    "bar"
                                ]
                            }
                            """
                            """
                            {
                                "bar": "bar"
                            }
                            """
                            True
                , test "missing required property is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "object",
                                "properties": {
                                    "foo": {
                                        "enum": [
                                            "foo"
                                        ]
                                    },
                                    "bar": {
                                        "enum": [
                                            "bar"
                                        ]
                                    }
                                },
                                "required": [
                                    "bar"
                                ]
                            }
                            """
                            """
                            {
                                "foo": "foo"
                            }
                            """
                            False
                , test "missing all properties is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "object",
                                "properties": {
                                    "foo": {
                                        "enum": [
                                            "foo"
                                        ]
                                    },
                                    "bar": {
                                        "enum": [
                                            "bar"
                                        ]
                                    }
                                },
                                "required": [
                                    "bar"
                                ]
                            }
                            """
                            """
                            {}
                            """
                            False
                ]
            ]
        , describe "items.json"
            [ describe "suite: a schema given for items"
                [ test "valid items" <|
                    \() ->
                        examine
                            """
                            {
                                "items": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            [
                                1,
                                2,
                                3
                            ]
                            """
                            True
                , test "wrong type of items" <|
                    \() ->
                        examine
                            """
                            {
                                "items": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            [
                                1,
                                "x"
                            ]
                            """
                            False
                , test "ignores non-arrays" <|
                    \() ->
                        examine
                            """
                            {
                                "items": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            {
                                "foo": "bar"
                            }
                            """
                            True
                , test "JavaScript pseudo-array is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "items": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            {
                                "0": "invalid",
                                "length": 1
                            }
                            """
                            True
                ]
            , describe "suite: an array of schemas for items"
                [ test "correct types" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "type": "string"
                                    }
                                ]
                            }
                            """
                            """
                            [
                                1,
                                "foo"
                            ]
                            """
                            True
                , test "wrong types" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "type": "string"
                                    }
                                ]
                            }
                            """
                            """
                            [
                                "foo",
                                1
                            ]
                            """
                            False
                , test "incomplete array of items" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "type": "string"
                                    }
                                ]
                            }
                            """
                            """
                            [
                                1
                            ]
                            """
                            True
                , test "array with additional items" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "type": "string"
                                    }
                                ]
                            }
                            """
                            """
                            [
                                1,
                                "foo",
                                true
                            ]
                            """
                            True
                , test "empty array" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "type": "string"
                                    }
                                ]
                            }
                            """
                            """
                            []
                            """
                            True
                , test "JavaScript pseudo-array is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "type": "string"
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "0": "invalid",
                                "1": "valid",
                                "length": 2
                            }
                            """
                            True
                ]
            ]
        , describe "maxItems.json"
            [ describe "suite: maxItems validation"
                [ test "shorter is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "maxItems": 2
                            }
                            """
                            """
                            [
                                1
                            ]
                            """
                            True
                , test "exact length is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "maxItems": 2
                            }
                            """
                            """
                            [
                                1,
                                2
                            ]
                            """
                            True
                , test "too long is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "maxItems": 2
                            }
                            """
                            """
                            [
                                1,
                                2,
                                3
                            ]
                            """
                            False
                , test "ignores non-arrays" <|
                    \() ->
                        examine
                            """
                            {
                                "maxItems": 2
                            }
                            """
                            """
                            "foobar"
                            """
                            True
                ]
            ]
        , describe "maxLength.json"
            [ describe "suite: maxLength validation"
                [ test "shorter is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "maxLength": 2
                            }
                            """
                            """
                            "f"
                            """
                            True
                , test "exact length is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "maxLength": 2
                            }
                            """
                            """
                            "fo"
                            """
                            True
                , test "too long is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "maxLength": 2
                            }
                            """
                            """
                            "foo"
                            """
                            False
                , test "ignores non-strings" <|
                    \() ->
                        examine
                            """
                            {
                                "maxLength": 2
                            }
                            """
                            """
                            100
                            """
                            True
                , test "two supplementary Unicode code points is long enough" <|
                    \() ->
                        examine
                            """
                            {
                                "maxLength": 2
                            }
                            """
                            """
                            "💩💩"
                            """
                            True
                ]
            ]
        , describe "maxProperties.json"
            [ describe "suite: maxProperties validation"
                [ test "shorter is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "maxProperties": 2
                            }
                            """
                            """
                            {
                                "foo": 1
                            }
                            """
                            True
                , test "exact length is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "maxProperties": 2
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2
                            }
                            """
                            True
                , test "too long is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "maxProperties": 2
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2,
                                "baz": 3
                            }
                            """
                            False
                , test "ignores arrays" <|
                    \() ->
                        examine
                            """
                            {
                                "maxProperties": 2
                            }
                            """
                            """
                            [
                                1,
                                2,
                                3
                            ]
                            """
                            True
                , test "ignores strings" <|
                    \() ->
                        examine
                            """
                            {
                                "maxProperties": 2
                            }
                            """
                            """
                            "foobar"
                            """
                            True
                , test "ignores other non-objects" <|
                    \() ->
                        examine
                            """
                            {
                                "maxProperties": 2
                            }
                            """
                            """
                            12
                            """
                            True
                ]
            ]
        , describe "maximum.json"
            [ describe "suite: maximum validation"
                [ test "below the maximum is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "maximum": 3
                            }
                            """
                            """
                            2.6
                            """
                            True
                , test "boundary point is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "maximum": 3
                            }
                            """
                            """
                            3
                            """
                            True
                , test "above the maximum is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "maximum": 3
                            }
                            """
                            """
                            3.5
                            """
                            False
                , test "ignores non-numbers" <|
                    \() ->
                        examine
                            """
                            {
                                "maximum": 3
                            }
                            """
                            """
                            "x"
                            """
                            True
                ]
            , describe "suite: exclusiveMaximum validation"
                [ test "below the maximum is still valid" <|
                    \() ->
                        examine
                            """
                            {
                                "maximum": 3,
                                "exclusiveMaximum": true
                            }
                            """
                            """
                            2.2
                            """
                            True
                , test "boundary point is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "maximum": 3,
                                "exclusiveMaximum": true
                            }
                            """
                            """
                            3
                            """
                            False
                ]
            ]
        , describe "minItems.json"
            [ describe "suite: minItems validation"
                [ test "longer is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "minItems": 1
                            }
                            """
                            """
                            [
                                1,
                                2
                            ]
                            """
                            True
                , test "exact length is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "minItems": 1
                            }
                            """
                            """
                            [
                                1
                            ]
                            """
                            True
                , test "too short is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "minItems": 1
                            }
                            """
                            """
                            []
                            """
                            False
                , test "ignores non-arrays" <|
                    \() ->
                        examine
                            """
                            {
                                "minItems": 1
                            }
                            """
                            """
                            ""
                            """
                            True
                ]
            ]
        , describe "minLength.json"
            [ describe "suite: minLength validation"
                [ test "longer is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "minLength": 2
                            }
                            """
                            """
                            "foo"
                            """
                            True
                , test "exact length is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "minLength": 2
                            }
                            """
                            """
                            "fo"
                            """
                            True
                , test "too short is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "minLength": 2
                            }
                            """
                            """
                            "f"
                            """
                            False
                , test "ignores non-strings" <|
                    \() ->
                        examine
                            """
                            {
                                "minLength": 2
                            }
                            """
                            """
                            1
                            """
                            True
                , test "one supplementary Unicode code point is not long enough" <|
                    \() ->
                        examine
                            """
                            {
                                "minLength": 2
                            }
                            """
                            """
                            "💩"
                            """
                            False
                ]
            ]
        , describe "minProperties.json"
            [ describe "suite: minProperties validation"
                [ test "longer is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "minProperties": 1
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2
                            }
                            """
                            True
                , test "exact length is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "minProperties": 1
                            }
                            """
                            """
                            {
                                "foo": 1
                            }
                            """
                            True
                , test "too short is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "minProperties": 1
                            }
                            """
                            """
                            {}
                            """
                            False
                , test "ignores arrays" <|
                    \() ->
                        examine
                            """
                            {
                                "minProperties": 1
                            }
                            """
                            """
                            []
                            """
                            True
                , test "ignores strings" <|
                    \() ->
                        examine
                            """
                            {
                                "minProperties": 1
                            }
                            """
                            """
                            ""
                            """
                            True
                , test "ignores other non-objects" <|
                    \() ->
                        examine
                            """
                            {
                                "minProperties": 1
                            }
                            """
                            """
                            12
                            """
                            True
                ]
            ]
        , describe "minimum.json"
            [ describe "suite: minimum validation"
                [ test "above the minimum is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "minimum": 1.1
                            }
                            """
                            """
                            2.6
                            """
                            True
                , test "boundary point is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "minimum": 1.1
                            }
                            """
                            """
                            1.1
                            """
                            True
                , test "below the minimum is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "minimum": 1.1
                            }
                            """
                            """
                            0.6
                            """
                            False
                , test "ignores non-numbers" <|
                    \() ->
                        examine
                            """
                            {
                                "minimum": 1.1
                            }
                            """
                            """
                            "x"
                            """
                            True
                ]
            , describe "suite: exclusiveMinimum validation"
                [ test "above the minimum is still valid" <|
                    \() ->
                        examine
                            """
                            {
                                "minimum": 1.1,
                                "exclusiveMinimum": true
                            }
                            """
                            """
                            1.2
                            """
                            True
                , test "boundary point is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "minimum": 1.1,
                                "exclusiveMinimum": true
                            }
                            """
                            """
                            1.1
                            """
                            False
                ]
            ]
        , describe "multipleOf.json"
            [ describe "suite: by int"
                [ test "int by int" <|
                    \() ->
                        examine
                            """
                            {
                                "multipleOf": 2
                            }
                            """
                            """
                            10
                            """
                            True
                , test "int by int fail" <|
                    \() ->
                        examine
                            """
                            {
                                "multipleOf": 2
                            }
                            """
                            """
                            7
                            """
                            False
                , test "ignores non-numbers" <|
                    \() ->
                        examine
                            """
                            {
                                "multipleOf": 2
                            }
                            """
                            """
                            "foo"
                            """
                            True
                ]
            , describe "suite: by number"
                [ test "zero is multiple of anything" <|
                    \() ->
                        examine
                            """
                            {
                                "multipleOf": 1.5
                            }
                            """
                            """
                            0
                            """
                            True
                , test "4.5 is multiple of 1.5" <|
                    \() ->
                        examine
                            """
                            {
                                "multipleOf": 1.5
                            }
                            """
                            """
                            4.5
                            """
                            True
                , test "35 is not multiple of 1.5" <|
                    \() ->
                        examine
                            """
                            {
                                "multipleOf": 1.5
                            }
                            """
                            """
                            35
                            """
                            False
                ]
            , describe "suite: by small number"
                [ test "0.0075 is multiple of 0.0001" <|
                    \() ->
                        examine
                            """
                            {
                                "multipleOf": 0.0001
                            }
                            """
                            """
                            0.0075
                            """
                            True
                , test "0.00751 is not multiple of 0.0001" <|
                    \() ->
                        examine
                            """
                            {
                                "multipleOf": 0.0001
                            }
                            """
                            """
                            0.00751
                            """
                            False
                ]
            ]
        , describe "not.json"
            [ describe "suite: not"
                [ test "allowed" <|
                    \() ->
                        examine
                            """
                            {
                                "not": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            "foo"
                            """
                            True
                , test "disallowed" <|
                    \() ->
                        examine
                            """
                            {
                                "not": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            1
                            """
                            False
                ]
            , describe "suite: not multiple types"
                [ test "valid" <|
                    \() ->
                        examine
                            """
                            {
                                "not": {
                                    "type": [
                                        "integer",
                                        "boolean"
                                    ]
                                }
                            }
                            """
                            """
                            "foo"
                            """
                            True
                , test "mismatch" <|
                    \() ->
                        examine
                            """
                            {
                                "not": {
                                    "type": [
                                        "integer",
                                        "boolean"
                                    ]
                                }
                            }
                            """
                            """
                            1
                            """
                            False
                , test "other mismatch" <|
                    \() ->
                        examine
                            """
                            {
                                "not": {
                                    "type": [
                                        "integer",
                                        "boolean"
                                    ]
                                }
                            }
                            """
                            """
                            true
                            """
                            False
                ]
            , describe "suite: not more complex schema"
                [ test "match" <|
                    \() ->
                        examine
                            """
                            {
                                "not": {
                                    "type": "object",
                                    "properties": {
                                        "foo": {
                                            "type": "string"
                                        }
                                    }
                                }
                            }
                            """
                            """
                            1
                            """
                            True
                , test "other match" <|
                    \() ->
                        examine
                            """
                            {
                                "not": {
                                    "type": "object",
                                    "properties": {
                                        "foo": {
                                            "type": "string"
                                        }
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": 1
                            }
                            """
                            True
                , test "mismatch" <|
                    \() ->
                        examine
                            """
                            {
                                "not": {
                                    "type": "object",
                                    "properties": {
                                        "foo": {
                                            "type": "string"
                                        }
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": "bar"
                            }
                            """
                            False
                ]
            , describe "suite: forbidden property"
                [ test "property present" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "not": {}
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": 2
                            }
                            """
                            False
                , test "property absent" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "not": {}
                                    }
                                }
                            }
                            """
                            """
                            {
                                "bar": 1,
                                "baz": 2
                            }
                            """
                            True
                ]
            ]
        , describe "oneOf.json"
            [ describe "suite: oneOf"
                [ test "first oneOf valid" <|
                    \() ->
                        examine
                            """
                            {
                                "oneOf": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "minimum": 2
                                    }
                                ]
                            }
                            """
                            """
                            1
                            """
                            True
                , test "second oneOf valid" <|
                    \() ->
                        examine
                            """
                            {
                                "oneOf": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "minimum": 2
                                    }
                                ]
                            }
                            """
                            """
                            2.5
                            """
                            True
                , test "both oneOf valid" <|
                    \() ->
                        examine
                            """
                            {
                                "oneOf": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "minimum": 2
                                    }
                                ]
                            }
                            """
                            """
                            3
                            """
                            False
                , test "neither oneOf valid" <|
                    \() ->
                        examine
                            """
                            {
                                "oneOf": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "minimum": 2
                                    }
                                ]
                            }
                            """
                            """
                            1.5
                            """
                            False
                ]
            , describe "suite: oneOf with base schema"
                [ test "mismatch base schema" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string",
                                "oneOf": [
                                    {
                                        "minLength": 2
                                    },
                                    {
                                        "maxLength": 4
                                    }
                                ]
                            }
                            """
                            """
                            3
                            """
                            False
                , test "one oneOf valid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string",
                                "oneOf": [
                                    {
                                        "minLength": 2
                                    },
                                    {
                                        "maxLength": 4
                                    }
                                ]
                            }
                            """
                            """
                            "foobar"
                            """
                            True
                , test "both oneOf valid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string",
                                "oneOf": [
                                    {
                                        "minLength": 2
                                    },
                                    {
                                        "maxLength": 4
                                    }
                                ]
                            }
                            """
                            """
                            "foo"
                            """
                            False
                ]
            , describe "suite: oneOf complex types"
                [ test "first oneOf valid (complex)" <|
                    \() ->
                        examine
                            """
                            {
                                "oneOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "bar": 2
                            }
                            """
                            True
                , test "second oneOf valid (complex)" <|
                    \() ->
                        examine
                            """
                            {
                                "oneOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": "baz"
                            }
                            """
                            True
                , test "both oneOf valid (complex)" <|
                    \() ->
                        examine
                            """
                            {
                                "oneOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": "baz",
                                "bar": 2
                            }
                            """
                            False
                , test "neither oneOf valid (complex)" <|
                    \() ->
                        examine
                            """
                            {
                                "oneOf": [
                                    {
                                        "properties": {
                                            "bar": {
                                                "type": "integer"
                                            }
                                        },
                                        "required": [
                                            "bar"
                                        ]
                                    },
                                    {
                                        "properties": {
                                            "foo": {
                                                "type": "string"
                                            }
                                        },
                                        "required": [
                                            "foo"
                                        ]
                                    }
                                ]
                            }
                            """
                            """
                            {
                                "foo": 2,
                                "bar": "quux"
                            }
                            """
                            False
                ]
            ]
        , describe "pattern.json"
            [ describe "suite: pattern validation"
                [ test "a matching pattern is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "pattern": "^a*$"
                            }
                            """
                            """
                            "aaa"
                            """
                            True
                , test "a non-matching pattern is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "pattern": "^a*$"
                            }
                            """
                            """
                            "abc"
                            """
                            False
                , test "ignores non-strings" <|
                    \() ->
                        examine
                            """
                            {
                                "pattern": "^a*$"
                            }
                            """
                            """
                            true
                            """
                            True
                ]
            , describe "suite: pattern is not anchored"
                [ test "matches a substring" <|
                    \() ->
                        examine
                            """
                            {
                                "pattern": "a+"
                            }
                            """
                            """
                            "xxaayy"
                            """
                            True
                ]
            ]
        , describe "patternProperties.json"
            [ describe "suite: patternProperties validates properties matching a regex"
                [ test "a single valid match is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "f.*o": {
                                        "type": "integer"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": 1
                            }
                            """
                            True
                , test "multiple valid matches is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "f.*o": {
                                        "type": "integer"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "foooooo": 2
                            }
                            """
                            True
                , test "a single invalid match is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "f.*o": {
                                        "type": "integer"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": "bar",
                                "fooooo": 2
                            }
                            """
                            False
                , test "multiple invalid matches is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "f.*o": {
                                        "type": "integer"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": "bar",
                                "foooooo": "baz"
                            }
                            """
                            False
                , test "ignores arrays" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "f.*o": {
                                        "type": "integer"
                                    }
                                }
                            }
                            """
                            """
                            []
                            """
                            True
                , test "ignores strings" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "f.*o": {
                                        "type": "integer"
                                    }
                                }
                            }
                            """
                            """
                            ""
                            """
                            True
                , test "ignores other non-objects" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "f.*o": {
                                        "type": "integer"
                                    }
                                }
                            }
                            """
                            """
                            12
                            """
                            True
                ]
            , describe "suite: multiple simultaneous patternProperties are validated"
                [ test "a single valid match is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "a*": {
                                        "type": "integer"
                                    },
                                    "aaa*": {
                                        "maximum": 20
                                    }
                                }
                            }
                            """
                            """
                            {
                                "a": 21
                            }
                            """
                            True
                , test "a simultaneous match is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "a*": {
                                        "type": "integer"
                                    },
                                    "aaa*": {
                                        "maximum": 20
                                    }
                                }
                            }
                            """
                            """
                            {
                                "aaaa": 18
                            }
                            """
                            True
                , test "multiple matches is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "a*": {
                                        "type": "integer"
                                    },
                                    "aaa*": {
                                        "maximum": 20
                                    }
                                }
                            }
                            """
                            """
                            {
                                "a": 21,
                                "aaaa": 18
                            }
                            """
                            True
                , test "an invalid due to one is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "a*": {
                                        "type": "integer"
                                    },
                                    "aaa*": {
                                        "maximum": 20
                                    }
                                }
                            }
                            """
                            """
                            {
                                "a": "bar"
                            }
                            """
                            False
                , test "an invalid due to the other is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "a*": {
                                        "type": "integer"
                                    },
                                    "aaa*": {
                                        "maximum": 20
                                    }
                                }
                            }
                            """
                            """
                            {
                                "aaaa": 31
                            }
                            """
                            False
                , test "an invalid due to both is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "a*": {
                                        "type": "integer"
                                    },
                                    "aaa*": {
                                        "maximum": 20
                                    }
                                }
                            }
                            """
                            """
                            {
                                "aaa": "foo",
                                "aaaa": 31
                            }
                            """
                            False
                ]
            , describe "suite: regexes are not anchored by default and are case sensitive"
                [ test "non recognized members are ignored" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "[0-9]{2,}": {
                                        "type": "boolean"
                                    },
                                    "X_": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "answer 1": "42"
                            }
                            """
                            True
                , test "recognized members are accounted for" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "[0-9]{2,}": {
                                        "type": "boolean"
                                    },
                                    "X_": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "a31b": null
                            }
                            """
                            False
                , test "regexes are case sensitive" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "[0-9]{2,}": {
                                        "type": "boolean"
                                    },
                                    "X_": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "a_x_3": 3
                            }
                            """
                            True
                , test "regexes are case sensitive, 2" <|
                    \() ->
                        examine
                            """
                            {
                                "patternProperties": {
                                    "[0-9]{2,}": {
                                        "type": "boolean"
                                    },
                                    "X_": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "a_X_3": 3
                            }
                            """
                            False
                ]
            ]
        , describe "properties.json"
            [ describe "suite: object properties validation"
                [ test "both properties present and valid is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "integer"
                                    },
                                    "bar": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": "baz"
                            }
                            """
                            True
                , test "one property invalid is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "integer"
                                    },
                                    "bar": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": 1,
                                "bar": {}
                            }
                            """
                            False
                , test "both properties invalid is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "integer"
                                    },
                                    "bar": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": [],
                                "bar": {}
                            }
                            """
                            False
                , test "doesn't invalidate other properties" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "integer"
                                    },
                                    "bar": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "quux": []
                            }
                            """
                            True
                , test "ignores arrays" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "integer"
                                    },
                                    "bar": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            []
                            """
                            True
                , test "ignores other non-objects" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "integer"
                                    },
                                    "bar": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            12
                            """
                            True
                ]
            , describe "suite: properties, patternProperties, additionalProperties interaction"
                [ test "property validates property" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "array",
                                        "maxItems": 3
                                    },
                                    "bar": {
                                        "type": "array"
                                    }
                                },
                                "patternProperties": {
                                    "f.o": {
                                        "minItems": 2
                                    }
                                },
                                "additionalProperties": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            {
                                "foo": [
                                    1,
                                    2
                                ]
                            }
                            """
                            True
                , test "property invalidates property" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "array",
                                        "maxItems": 3
                                    },
                                    "bar": {
                                        "type": "array"
                                    }
                                },
                                "patternProperties": {
                                    "f.o": {
                                        "minItems": 2
                                    }
                                },
                                "additionalProperties": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            {
                                "foo": [
                                    1,
                                    2,
                                    3,
                                    4
                                ]
                            }
                            """
                            False
                , test "patternProperty invalidates property" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "array",
                                        "maxItems": 3
                                    },
                                    "bar": {
                                        "type": "array"
                                    }
                                },
                                "patternProperties": {
                                    "f.o": {
                                        "minItems": 2
                                    }
                                },
                                "additionalProperties": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            {
                                "foo": []
                            }
                            """
                            False
                , test "patternProperty validates nonproperty" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "array",
                                        "maxItems": 3
                                    },
                                    "bar": {
                                        "type": "array"
                                    }
                                },
                                "patternProperties": {
                                    "f.o": {
                                        "minItems": 2
                                    }
                                },
                                "additionalProperties": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            {
                                "fxo": [
                                    1,
                                    2
                                ]
                            }
                            """
                            True
                , test "patternProperty invalidates nonproperty" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "array",
                                        "maxItems": 3
                                    },
                                    "bar": {
                                        "type": "array"
                                    }
                                },
                                "patternProperties": {
                                    "f.o": {
                                        "minItems": 2
                                    }
                                },
                                "additionalProperties": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            {
                                "fxo": []
                            }
                            """
                            False
                , test "additionalProperty ignores property" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "array",
                                        "maxItems": 3
                                    },
                                    "bar": {
                                        "type": "array"
                                    }
                                },
                                "patternProperties": {
                                    "f.o": {
                                        "minItems": 2
                                    }
                                },
                                "additionalProperties": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            {
                                "bar": []
                            }
                            """
                            True
                , test "additionalProperty validates others" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "array",
                                        "maxItems": 3
                                    },
                                    "bar": {
                                        "type": "array"
                                    }
                                },
                                "patternProperties": {
                                    "f.o": {
                                        "minItems": 2
                                    }
                                },
                                "additionalProperties": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            {
                                "quux": 3
                            }
                            """
                            True
                , test "additionalProperty invalidates others" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "array",
                                        "maxItems": 3
                                    },
                                    "bar": {
                                        "type": "array"
                                    }
                                },
                                "patternProperties": {
                                    "f.o": {
                                        "minItems": 2
                                    }
                                },
                                "additionalProperties": {
                                    "type": "integer"
                                }
                            }
                            """
                            """
                            {
                                "quux": "foo"
                            }
                            """
                            False
                ]
            ]
        , describe "ref.json"
            [ describe "suite: root pointer ref"
                [ test "match" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "$ref": "#"
                                    }
                                },
                                "additionalProperties": false
                            }
                            """
                            """
                            {
                                "foo": false
                            }
                            """
                            True
                , test "recursive match" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "$ref": "#"
                                    }
                                },
                                "additionalProperties": false
                            }
                            """
                            """
                            {
                                "foo": {
                                    "foo": false
                                }
                            }
                            """
                            True
                , test "mismatch" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "$ref": "#"
                                    }
                                },
                                "additionalProperties": false
                            }
                            """
                            """
                            {
                                "bar": false
                            }
                            """
                            False
                , test "recursive mismatch" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "$ref": "#"
                                    }
                                },
                                "additionalProperties": false
                            }
                            """
                            """
                            {
                                "foo": {
                                    "bar": false
                                }
                            }
                            """
                            False
                ]
            , describe "suite: relative pointer ref to object"
                [ test "match" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "integer"
                                    },
                                    "bar": {
                                        "$ref": "#/properties/foo"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "bar": 3
                            }
                            """
                            True
                , test "mismatch" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {
                                        "type": "integer"
                                    },
                                    "bar": {
                                        "$ref": "#/properties/foo"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "bar": true
                            }
                            """
                            False
                ]
            , describe "suite: relative pointer ref to array"
                [ test "match array" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "$ref": "#/items/0"
                                    }
                                ]
                            }
                            """
                            """
                            [
                                1,
                                2
                            ]
                            """
                            True
                , test "mismatch array" <|
                    \() ->
                        examine
                            """
                            {
                                "items": [
                                    {
                                        "type": "integer"
                                    },
                                    {
                                        "$ref": "#/items/0"
                                    }
                                ]
                            }
                            """
                            """
                            [
                                1,
                                "foo"
                            ]
                            """
                            False
                ]
            , describe "suite: escaped pointer ref"
                [ test "slash invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "tilda~field": {
                                    "type": "integer"
                                },
                                "slash/field": {
                                    "type": "integer"
                                },
                                "percent%field": {
                                    "type": "integer"
                                },
                                "properties": {
                                    "tilda": {
                                        "$ref": "#/tilda~0field"
                                    },
                                    "slash": {
                                        "$ref": "#/slash~1field"
                                    },
                                    "percent": {
                                        "$ref": "#/percent%25field"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "slash": "aoeu"
                            }
                            """
                            False
                , test "tilda invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "tilda~field": {
                                    "type": "integer"
                                },
                                "slash/field": {
                                    "type": "integer"
                                },
                                "percent%field": {
                                    "type": "integer"
                                },
                                "properties": {
                                    "tilda": {
                                        "$ref": "#/tilda~0field"
                                    },
                                    "slash": {
                                        "$ref": "#/slash~1field"
                                    },
                                    "percent": {
                                        "$ref": "#/percent%25field"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "tilda": "aoeu"
                            }
                            """
                            False
                , test "percent invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "tilda~field": {
                                    "type": "integer"
                                },
                                "slash/field": {
                                    "type": "integer"
                                },
                                "percent%field": {
                                    "type": "integer"
                                },
                                "properties": {
                                    "tilda": {
                                        "$ref": "#/tilda~0field"
                                    },
                                    "slash": {
                                        "$ref": "#/slash~1field"
                                    },
                                    "percent": {
                                        "$ref": "#/percent%25field"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "percent": "aoeu"
                            }
                            """
                            False
                , test "slash valid" <|
                    \() ->
                        examine
                            """
                            {
                                "tilda~field": {
                                    "type": "integer"
                                },
                                "slash/field": {
                                    "type": "integer"
                                },
                                "percent%field": {
                                    "type": "integer"
                                },
                                "properties": {
                                    "tilda": {
                                        "$ref": "#/tilda~0field"
                                    },
                                    "slash": {
                                        "$ref": "#/slash~1field"
                                    },
                                    "percent": {
                                        "$ref": "#/percent%25field"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "slash": 123
                            }
                            """
                            True
                , test "tilda valid" <|
                    \() ->
                        examine
                            """
                            {
                                "tilda~field": {
                                    "type": "integer"
                                },
                                "slash/field": {
                                    "type": "integer"
                                },
                                "percent%field": {
                                    "type": "integer"
                                },
                                "properties": {
                                    "tilda": {
                                        "$ref": "#/tilda~0field"
                                    },
                                    "slash": {
                                        "$ref": "#/slash~1field"
                                    },
                                    "percent": {
                                        "$ref": "#/percent%25field"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "tilda": 123
                            }
                            """
                            True
                , test "percent valid" <|
                    \() ->
                        examine
                            """
                            {
                                "tilda~field": {
                                    "type": "integer"
                                },
                                "slash/field": {
                                    "type": "integer"
                                },
                                "percent%field": {
                                    "type": "integer"
                                },
                                "properties": {
                                    "tilda": {
                                        "$ref": "#/tilda~0field"
                                    },
                                    "slash": {
                                        "$ref": "#/slash~1field"
                                    },
                                    "percent": {
                                        "$ref": "#/percent%25field"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "percent": 123
                            }
                            """
                            True
                ]
            , describe "suite: nested refs"
                [ test "nested ref valid" <|
                    \() ->
                        examine
                            """
                            {
                                "definitions": {
                                    "a": {
                                        "type": "integer"
                                    },
                                    "b": {
                                        "$ref": "#/definitions/a"
                                    },
                                    "c": {
                                        "$ref": "#/definitions/b"
                                    }
                                },
                                "$ref": "#/definitions/c"
                            }
                            """
                            """
                            5
                            """
                            True
                , test "nested ref invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "definitions": {
                                    "a": {
                                        "type": "integer"
                                    },
                                    "b": {
                                        "$ref": "#/definitions/a"
                                    },
                                    "c": {
                                        "$ref": "#/definitions/b"
                                    }
                                },
                                "$ref": "#/definitions/c"
                            }
                            """
                            """
                            "a"
                            """
                            False
                ]
            , describe "suite: ref overrides any sibling keywords"
                [ test "ref valid" <|
                    \() ->
                        examine
                            """
                            {
                                "definitions": {
                                    "reffed": {
                                        "type": "array"
                                    }
                                },
                                "properties": {
                                    "foo": {
                                        "$ref": "#/definitions/reffed",
                                        "maxItems": 2
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": []
                            }
                            """
                            True
                , test "ref valid, maxItems ignored" <|
                    \() ->
                        examine
                            """
                            {
                                "definitions": {
                                    "reffed": {
                                        "type": "array"
                                    }
                                },
                                "properties": {
                                    "foo": {
                                        "$ref": "#/definitions/reffed",
                                        "maxItems": 2
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": [
                                    1,
                                    2,
                                    3
                                ]
                            }
                            """
                            True
                , test "ref invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "definitions": {
                                    "reffed": {
                                        "type": "array"
                                    }
                                },
                                "properties": {
                                    "foo": {
                                        "$ref": "#/definitions/reffed",
                                        "maxItems": 2
                                    }
                                }
                            }
                            """
                            """
                            {
                                "foo": "string"
                            }
                            """
                            False
                ]
            , describe "suite: remote ref, containing refs itself"
                [ test "remote ref valid" <|
                    \() ->
                        examine
                            """
                            {
                                "$ref": "http://json-schema.org/draft-04/schema#"
                            }
                            """
                            """
                            {
                                "minLength": 1
                            }
                            """
                            True
                , test "remote ref invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "$ref": "http://json-schema.org/draft-04/schema#"
                            }
                            """
                            """
                            {
                                "minLength": -1
                            }
                            """
                            False
                ]
            , describe "suite: property named $ref that is not a reference"
                [ test "property named $ref valid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "$ref": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "$ref": "a"
                            }
                            """
                            True
                , test "property named $ref invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "$ref": {
                                        "type": "string"
                                    }
                                }
                            }
                            """
                            """
                            {
                                "$ref": 2
                            }
                            """
                            False
                ]
            , describe "suite: Recursive references between schemas"
                [ test "valid tree" <|
                    \() ->
                        examine
                            """
                            {
                                "id": "http://localhost:1234/tree",
                                "description": "tree of nodes",
                                "type": "object",
                                "properties": {
                                    "meta": {
                                        "type": "string"
                                    },
                                    "nodes": {
                                        "type": "array",
                                        "items": {
                                            "$ref": "node"
                                        }
                                    }
                                },
                                "required": [
                                    "meta",
                                    "nodes"
                                ],
                                "definitions": {
                                    "node": {
                                        "id": "http://localhost:1234/node",
                                        "description": "node",
                                        "type": "object",
                                        "properties": {
                                            "value": {
                                                "type": "number"
                                            },
                                            "subtree": {
                                                "$ref": "tree"
                                            }
                                        },
                                        "required": [
                                            "value"
                                        ]
                                    }
                                }
                            }
                            """
                            """
                            {
                                "meta": "root",
                                "nodes": [
                                    {
                                        "value": 1,
                                        "subtree": {
                                            "meta": "child",
                                            "nodes": [
                                                {
                                                    "value": 1.1
                                                },
                                                {
                                                    "value": 1.2
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "value": 2,
                                        "subtree": {
                                            "meta": "child",
                                            "nodes": [
                                                {
                                                    "value": 2.1
                                                },
                                                {
                                                    "value": 2.2
                                                }
                                            ]
                                        }
                                    }
                                ]
                            }
                            """
                            True
                , test "invalid tree" <|
                    \() ->
                        examine
                            """
                            {
                                "id": "http://localhost:1234/tree",
                                "description": "tree of nodes",
                                "type": "object",
                                "properties": {
                                    "meta": {
                                        "type": "string"
                                    },
                                    "nodes": {
                                        "type": "array",
                                        "items": {
                                            "$ref": "node"
                                        }
                                    }
                                },
                                "required": [
                                    "meta",
                                    "nodes"
                                ],
                                "definitions": {
                                    "node": {
                                        "id": "http://localhost:1234/node",
                                        "description": "node",
                                        "type": "object",
                                        "properties": {
                                            "value": {
                                                "type": "number"
                                            },
                                            "subtree": {
                                                "$ref": "tree"
                                            }
                                        },
                                        "required": [
                                            "value"
                                        ]
                                    }
                                }
                            }
                            """
                            """
                            {
                                "meta": "root",
                                "nodes": [
                                    {
                                        "value": 1,
                                        "subtree": {
                                            "meta": "child",
                                            "nodes": [
                                                {
                                                    "value": "string is invalid"
                                                },
                                                {
                                                    "value": 1.2
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "value": 2,
                                        "subtree": {
                                            "meta": "child",
                                            "nodes": [
                                                {
                                                    "value": 2.1
                                                },
                                                {
                                                    "value": 2.2
                                                }
                                            ]
                                        }
                                    }
                                ]
                            }
                            """
                            False
                ]
            ]
        , describe "required.json"
            [ describe "suite: required validation"
                [ test "present required property is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "required": [
                                    "foo"
                                ]
                            }
                            """
                            """
                            {
                                "foo": 1
                            }
                            """
                            True
                , test "non-present required property is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "required": [
                                    "foo"
                                ]
                            }
                            """
                            """
                            {
                                "bar": 1
                            }
                            """
                            False
                , test "ignores arrays" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "required": [
                                    "foo"
                                ]
                            }
                            """
                            """
                            []
                            """
                            True
                , test "ignores strings" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "required": [
                                    "foo"
                                ]
                            }
                            """
                            """
                            ""
                            """
                            True
                , test "ignores other non-objects" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {},
                                    "bar": {}
                                },
                                "required": [
                                    "foo"
                                ]
                            }
                            """
                            """
                            12
                            """
                            True
                ]
            , describe "suite: required default validation"
                [ test "not required by default" <|
                    \() ->
                        examine
                            """
                            {
                                "properties": {
                                    "foo": {}
                                }
                            }
                            """
                            """
                            {}
                            """
                            True
                ]
            ]
        , describe "type.json"
            [ describe "suite: integer type matches integers"
                [ test "an integer is an integer" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "integer"
                            }
                            """
                            """
                            1
                            """
                            True
                , test "a float is not an integer" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "integer"
                            }
                            """
                            """
                            1.1
                            """
                            False
                , test "a string is not an integer" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "integer"
                            }
                            """
                            """
                            "foo"
                            """
                            False
                , test "a string is still not an integer, even if it looks like one" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "integer"
                            }
                            """
                            """
                            "1"
                            """
                            False
                , test "an object is not an integer" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "integer"
                            }
                            """
                            """
                            {}
                            """
                            False
                , test "an array is not an integer" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "integer"
                            }
                            """
                            """
                            []
                            """
                            False
                , test "a boolean is not an integer" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "integer"
                            }
                            """
                            """
                            true
                            """
                            False
                , test "null is not an integer" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "integer"
                            }
                            """
                            """
                            null
                            """
                            False
                ]
            , describe "suite: number type matches numbers"
                [ test "an integer is a number" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "number"
                            }
                            """
                            """
                            1
                            """
                            True
                , test "a float is a number" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "number"
                            }
                            """
                            """
                            1.1
                            """
                            True
                , test "a string is not a number" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "number"
                            }
                            """
                            """
                            "foo"
                            """
                            False
                , test "a string is still not a number, even if it looks like one" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "number"
                            }
                            """
                            """
                            "1"
                            """
                            False
                , test "an object is not a number" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "number"
                            }
                            """
                            """
                            {}
                            """
                            False
                , test "an array is not a number" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "number"
                            }
                            """
                            """
                            []
                            """
                            False
                , test "a boolean is not a number" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "number"
                            }
                            """
                            """
                            true
                            """
                            False
                , test "null is not a number" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "number"
                            }
                            """
                            """
                            null
                            """
                            False
                ]
            , describe "suite: string type matches strings"
                [ test "1 is not a string" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string"
                            }
                            """
                            """
                            1
                            """
                            False
                , test "a float is not a string" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string"
                            }
                            """
                            """
                            1.1
                            """
                            False
                , test "a string is a string" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string"
                            }
                            """
                            """
                            "foo"
                            """
                            True
                , test "a string is still a string, even if it looks like a number" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string"
                            }
                            """
                            """
                            "1"
                            """
                            True
                , test "an object is not a string" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string"
                            }
                            """
                            """
                            {}
                            """
                            False
                , test "an array is not a string" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string"
                            }
                            """
                            """
                            []
                            """
                            False
                , test "a boolean is not a string" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string"
                            }
                            """
                            """
                            true
                            """
                            False
                , test "null is not a string" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "string"
                            }
                            """
                            """
                            null
                            """
                            False
                ]
            , describe "suite: object type matches objects"
                [ test "an integer is not an object" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "object"
                            }
                            """
                            """
                            1
                            """
                            False
                , test "a float is not an object" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "object"
                            }
                            """
                            """
                            1.1
                            """
                            False
                , test "a string is not an object" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "object"
                            }
                            """
                            """
                            "foo"
                            """
                            False
                , test "an object is an object" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "object"
                            }
                            """
                            """
                            {}
                            """
                            True
                , test "an array is not an object" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "object"
                            }
                            """
                            """
                            []
                            """
                            False
                , test "a boolean is not an object" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "object"
                            }
                            """
                            """
                            true
                            """
                            False
                , test "null is not an object" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "object"
                            }
                            """
                            """
                            null
                            """
                            False
                ]
            , describe "suite: array type matches arrays"
                [ test "an integer is not an array" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "array"
                            }
                            """
                            """
                            1
                            """
                            False
                , test "a float is not an array" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "array"
                            }
                            """
                            """
                            1.1
                            """
                            False
                , test "a string is not an array" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "array"
                            }
                            """
                            """
                            "foo"
                            """
                            False
                , test "an object is not an array" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "array"
                            }
                            """
                            """
                            {}
                            """
                            False
                , test "an array is an array" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "array"
                            }
                            """
                            """
                            []
                            """
                            True
                , test "a boolean is not an array" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "array"
                            }
                            """
                            """
                            true
                            """
                            False
                , test "null is not an array" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "array"
                            }
                            """
                            """
                            null
                            """
                            False
                ]
            , describe "suite: boolean type matches booleans"
                [ test "an integer is not a boolean" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "boolean"
                            }
                            """
                            """
                            1
                            """
                            False
                , test "a float is not a boolean" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "boolean"
                            }
                            """
                            """
                            1.1
                            """
                            False
                , test "a string is not a boolean" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "boolean"
                            }
                            """
                            """
                            "foo"
                            """
                            False
                , test "an object is not a boolean" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "boolean"
                            }
                            """
                            """
                            {}
                            """
                            False
                , test "an array is not a boolean" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "boolean"
                            }
                            """
                            """
                            []
                            """
                            False
                , test "a boolean is a boolean" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "boolean"
                            }
                            """
                            """
                            true
                            """
                            True
                , test "null is not a boolean" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "boolean"
                            }
                            """
                            """
                            null
                            """
                            False
                ]
            , describe "suite: null type matches only the null object"
                [ test "an integer is not null" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "null"
                            }
                            """
                            """
                            1
                            """
                            False
                , test "a float is not null" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "null"
                            }
                            """
                            """
                            1.1
                            """
                            False
                , test "a string is not null" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "null"
                            }
                            """
                            """
                            "foo"
                            """
                            False
                , test "an object is not null" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "null"
                            }
                            """
                            """
                            {}
                            """
                            False
                , test "an array is not null" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "null"
                            }
                            """
                            """
                            []
                            """
                            False
                , test "a boolean is not null" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "null"
                            }
                            """
                            """
                            true
                            """
                            False
                , test "null is null" <|
                    \() ->
                        examine
                            """
                            {
                                "type": "null"
                            }
                            """
                            """
                            null
                            """
                            True
                ]
            , describe "suite: multiple types can be specified in an array"
                [ test "an integer is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": [
                                    "integer",
                                    "string"
                                ]
                            }
                            """
                            """
                            1
                            """
                            True
                , test "a string is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": [
                                    "integer",
                                    "string"
                                ]
                            }
                            """
                            """
                            "foo"
                            """
                            True
                , test "a float is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": [
                                    "integer",
                                    "string"
                                ]
                            }
                            """
                            """
                            1.1
                            """
                            False
                , test "an object is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": [
                                    "integer",
                                    "string"
                                ]
                            }
                            """
                            """
                            {}
                            """
                            False
                , test "an array is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": [
                                    "integer",
                                    "string"
                                ]
                            }
                            """
                            """
                            []
                            """
                            False
                , test "a boolean is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": [
                                    "integer",
                                    "string"
                                ]
                            }
                            """
                            """
                            true
                            """
                            False
                , test "null is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "type": [
                                    "integer",
                                    "string"
                                ]
                            }
                            """
                            """
                            null
                            """
                            False
                ]
            ]
        , describe "uniqueItems.json"
            [ describe "suite: uniqueItems validation"
                [ test "unique array of integers is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                1,
                                2
                            ]
                            """
                            True
                , test "non-unique array of integers is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                1,
                                1
                            ]
                            """
                            False
                , test "numbers are unique if mathematically unequal" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                1,
                                1,
                                1
                            ]
                            """
                            False
                , test "unique array of objects is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                {
                                    "foo": "bar"
                                },
                                {
                                    "foo": "baz"
                                }
                            ]
                            """
                            True
                , test "non-unique array of objects is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                {
                                    "foo": "bar"
                                },
                                {
                                    "foo": "bar"
                                }
                            ]
                            """
                            False
                , test "unique array of nested objects is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                {
                                    "foo": {
                                        "bar": {
                                            "baz": true
                                        }
                                    }
                                },
                                {
                                    "foo": {
                                        "bar": {
                                            "baz": false
                                        }
                                    }
                                }
                            ]
                            """
                            True
                , test "non-unique array of nested objects is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                {
                                    "foo": {
                                        "bar": {
                                            "baz": true
                                        }
                                    }
                                },
                                {
                                    "foo": {
                                        "bar": {
                                            "baz": true
                                        }
                                    }
                                }
                            ]
                            """
                            False
                , test "unique array of arrays is valid" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                [
                                    "foo"
                                ],
                                [
                                    "bar"
                                ]
                            ]
                            """
                            True
                , test "non-unique array of arrays is invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                [
                                    "foo"
                                ],
                                [
                                    "foo"
                                ]
                            ]
                            """
                            False
                , test "1 and true are unique" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                1,
                                true
                            ]
                            """
                            True
                , test "0 and false are unique" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                0,
                                false
                            ]
                            """
                            True
                , test "unique heterogeneous types are valid" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                {},
                                [
                                    1
                                ],
                                true,
                                null,
                                1
                            ]
                            """
                            True
                , test "non-unique heterogeneous types are invalid" <|
                    \() ->
                        examine
                            """
                            {
                                "uniqueItems": true
                            }
                            """
                            """
                            [
                                {},
                                [
                                    1
                                ],
                                true,
                                null,
                                {},
                                1
                            ]
                            """
                            False
                ]
            ]
        ]


examine : String -> String -> Bool -> Expect.Expectation
examine schemaSource dataSource outcome =
    let
        schema =
            schemaSource
                |> decodeString decoder
                |> Result.withDefault blankSchema

        data =
            dataSource
                |> decodeString value
                |> Result.withDefault Encode.null

        result =
            validateValue { applyDefaults = False } data schema
                |> Result.mapError toString
                |> Result.map (\_ -> True)
    in
        if outcome then
            result
                |> Expect.equal (Ok True)
        else
            case result of
                Ok x ->
                    Expect.fail "Unexpected success"

                Err _ ->
                    Expect.pass
