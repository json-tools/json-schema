module Json.Schema.Examples exposing (coreSchemaDraft6, bookingSchema)


coreSchemaDraft6 : String
coreSchemaDraft6 =
    """
{
    "$schema": "http://json-schema.org/draft-06/schema#",
    "$id": "http://json-schema.org/draft-06/schema#",
    "title": "Core schema meta-schema",
    "definitions": {
        "schemaArray": {
            "type": "array",
            "minItems": 1,
            "items": { "$ref": "#" }
        },
        "nonNegativeInteger": {
            "type": "integer",
            "minimum": 0
        },
        "nonNegativeIntegerDefault0": {
            "allOf": [
                { "$ref": "#/definitions/nonNegativeInteger" },
                { "default": 0 }
            ]
        },
        "simpleTypes": {
            "enum": [
                "array",
                "boolean",
                "integer",
                "null",
                "number",
                "object",
                "string"
            ]
        },
        "stringArray": {
            "type": "array",
            "items": { "type": "string" },
            "uniqueItems": true,
            "default": []
        }
    },
    "type": ["object", "boolean"],
    "properties": {
        "$id": {
            "type": "string",
            "format": "uri-reference"
        },
        "$schema": {
            "type": "string",
            "format": "uri"
        },
        "$ref": {
            "type": "string",
            "format": "uri-reference"
        },
        "title": {
            "type": "string"
        },
        "description": {
            "type": "string"
        },
        "default": {},
        "multipleOf": {
            "type": "number",
            "exclusiveMinimum": 0
        },
        "maximum": {
            "type": "number"
        },
        "exclusiveMaximum": {
            "type": "number"
        },
        "minimum": {
            "type": "number"
        },
        "exclusiveMinimum": {
            "type": "number"
        },
        "maxLength": { "$ref": "#/definitions/nonNegativeInteger" },
        "minLength": { "$ref": "#/definitions/nonNegativeIntegerDefault0" },
        "pattern": {
            "type": "string",
            "format": "regex"
        },
        "additionalItems": { "$ref": "#" },
        "items": {
            "anyOf": [
                { "$ref": "#" },
                { "$ref": "#/definitions/schemaArray" }
            ],
            "default": {}
        },
        "maxItems": { "$ref": "#/definitions/nonNegativeInteger" },
        "minItems": { "$ref": "#/definitions/nonNegativeIntegerDefault0" },
        "uniqueItems": {
            "type": "boolean",
            "default": false
        },
        "contains": { "$ref": "#" },
        "maxProperties": { "$ref": "#/definitions/nonNegativeInteger" },
        "minProperties": { "$ref": "#/definitions/nonNegativeIntegerDefault0" },
        "required": { "$ref": "#/definitions/stringArray" },
        "additionalProperties": { "$ref": "#" },
        "definitions": {
            "type": "object",
            "additionalProperties": { "$ref": "#" },
            "default": {}
        },
        "properties": {
            "type": "object",
            "additionalProperties": { "$ref": "#" },
            "default": {}
        },
        "patternProperties": {
            "type": "object",
            "additionalProperties": { "$ref": "#" },
            "propertyNames": { "format": "regex" },
            "default": {}
        },
        "dependencies": {
            "type": "object",
            "additionalProperties": {
                "anyOf": [
                    { "$ref": "#" },
                    { "$ref": "#/definitions/stringArray" }
                ]
            }
        },
        "propertyNames": { "$ref": "#" },
        "const": {},
        "enum": {
            "type": "array",
            "minItems": 1,
            "uniqueItems": true
        },
        "type": {
            "anyOf": [
                { "$ref": "#/definitions/simpleTypes" },
                {
                    "type": "array",
                    "items": { "$ref": "#/definitions/simpleTypes" },
                    "minItems": 1,
                    "uniqueItems": true
                }
            ]
        },
        "format": { "type": "string" },
        "allOf": { "$ref": "#/definitions/schemaArray" },
        "anyOf": { "$ref": "#/definitions/schemaArray" },
        "oneOf": { "$ref": "#/definitions/schemaArray" },
        "not": { "$ref": "#" }
    },
    "default": {}
}
"""


bookingSchema : String
bookingSchema =
    """
{
 "$schema": "http://json-schema.org/draft-04/schema#",
 "type": "object",
 "properties": {
  "url": {
   "type": "string",
   "format": "uri"
  },
  "account": {
   "type": "object",
   "properties": {
    "email": {
     "type": "string",
     "format": "email"
    },
    "password": {
     "type": "string",
     "format": "string"
    },
    "phone": {
     "$ref": "#/definitions/phone"
    },
    "isExisting": {
     "type": "boolean",
     "enum": [
      true,
      false
     ]
    }
   },
   "required": [
    "email",
    "phone",
    "isExisting"
   ]
  },
  "flight": {
   "type": "object",
   "properties": {
    "from": {
     "$ref": "#/definitions/datePlace"
    },
    "to": {
     "$ref": "#/definitions/datePlace"
    },
    "return": {
     "type": "object",
     "properties": {
      "from": {
       "$ref": "#/definitions/datePlace"
      },
      "to": {
       "$ref": "#/definitions/datePlace"
      }
     },
     "required": [
      "from",
      "to"
     ]
    },
    "price": {
     "$ref": "#/definitions/price"
    },
    "cabinClass": {
     "type": "string",
     "enum": [
      "economy",
      "economy premium",
      "business",
      "first"
     ]
    }
   },
   "required": [
    "cabinClass",
    "from",
    "to",
    "price"
   ],
   "additionalProperties": true
  },
  "passengers": {
   "type": "array",
   "minItems": 1,
   "maxItems": 1,
   "items": {
    "type": "object",
    "properties": {
     "title": {
      "enum": [
       "mr",
       "miss",
       "ms",
       "mrs"
      ]
     },
     "firstName": {
      "type": "string"
     },
     "lastName": {
      "type": "string"
     },
     "dateOfBirth": {
      "type": "string",
      "format": "date"
     },
     "hasHoldLuggage": {
      "type": "boolean",
      "enum": [
       true,
       false
      ]
     },
     "id": {
      "type": "object",
      "properties": {
       "type": {
        "type": "string"
       },
       "number": {
        "type": "string"
       },
       "expDate": {
        "type": "string",
        "format": "date"
       },
       "countryCode": {
        "$ref": "#/definitions/countryCode"
       }
      }
     }
    },
    "required": [
     "title",
     "firstName",
     "lastName",
     "dateOfBirth",
     "hasHoldLuggage"
    ]
   }
  },
  "payment": {
   "type": "object",
   "properties": {
    "card": {
     "$ref": "#/definitions/paymentCard"
    },
    "address": {
     "$ref": "#/definitions/personAddress"
    }
   },
   "required": [
    "card",
    "address"
   ]
  }
 },
 "required": [
  "url",
  "account",
  "passengers",
  "payment",
  "flight"
 ],
 "definitions": {
  "basicPerson": {
   "type": "object",
   "properties": {
    "title": {
     "enum": [
      "mr",
      "miss",
      "ms",
      "mrs"
     ]
    },
    "firstName": {
     "type": "string"
    },
    "lastName": {
     "type": "string"
    }
   },
   "required": [
    "title",
    "firstName",
    "lastName"
   ]
  },
  "personAddress": {
   "allOf": [
    {
     "$ref": "#/definitions/basicPerson"
    },
    {
     "$ref": "#/definitions/address"
    }
   ]
  },
  "phone": {
   "type": "object",
   "properties": {
    "countryCode": {
     "$ref": "#/definitions/countryCode"
    },
    "number": {
     "type": "string",
     "minLength": 10,
     "description": "Mobile phone number (numbers only, excluding country code)"
    }
   },
   "required": [
    "countryCode",
    "number"
   ]
  },
  "price": {
   "type": "object",
   "properties": {
    "currencyCode": {
     "$ref": "#/definitions/currencyCode"
    },
    "value": {
     "type": "integer",
     "minimum": 0,
     "description": "A positive integer in the smallest currency unit (that is, 100 pence for £1.00)"
    }
   },
   "required": [
    "currencyCode",
    "value"
   ],
   "additionalProperties": false
  },
  "paymentCard": {
   "type": "object",
   "properties": {
    "type": {
     "type": "string",
     "enum": [
      "debit",
      "credit"
     ]
    },
    "brand": {
     "type": "string",
     "enum": [
      "visa",
      "mastercard",
      "amex"
     ]
    },
    "panToken": {
     "type": "string",
     "minLength": 20
    },
    "expirationDate": {
     "type": "string",
     "pattern": "^20[0-9]{2}-(?:0[1-9]|1[0-2])$",
     "minLength": 7,
     "maxLength": 7
    },
    "name": {
     "type": "string"
    },
    "cvv": {
     "type": "string",
     "minLength": 3,
     "maxLength": 4
    }
   },
   "required": [
    "type",
    "brand",
    "expirationDate",
    "name",
    "cvv"
   ]
  },
  "address": {
   "type": "object",
   "properties": {
    "line1": {
     "type": "string",
     "title": "Address line 1",
     "description": "Street name with house number"
    },
    "line2": {
     "type": "string",
     "title": "Address line 2",
     "description": "Additional address info"
    },
    "city": {
     "type": "string"
    },
    "postcode": {
     "type": "string"
    },
    "countryCode": {
     "$ref": "#/definitions/countryCode"
    }
   },
   "required": [
    "line1",
    "city",
    "postcode",
    "countryCode"
   ]
  },
  "datePlace": {
   "type": "object",
   "properties": {
    "countryCode": {
     "$ref": "#/definitions/countryCode"
    },
    "dateTime": {
     "type": "string",
     "pattern": "^20[0-9]{2}-(?:0[1-9]|1[0-2])-(?:0[1-9]|[1-3][0-9]) [012][0-9]:[0-5][0-9]$",
     "title": "Date-Time",
     "description": "Date and time of flight (airport local time)"
    },
    "airportCode": {
     "type": "string",
     "minLength": 3,
     "maxLength": 3,
     "pattern": "^[A-Z]{3}$",
     "title": "Airport Code",
     "description": "International Air Transport Association airport code"
    }
   },
   "required": [
    "dateTime",
    "airportCode"
   ],
   "propertyNames": { "enum": [ "dateTime", "airportCode", "countryCode" ] }
  },
  "currencyCode": {
   "type": "string",
   "minLength": 3,
   "maxLength": 3,
   "enum": [
    "all",
    "afn",
    "ars",
    "awg",
    "aud",
    "azn",
    "bsd",
    "bbd",
    "byn",
    "bzd",
    "bmd",
    "bob",
    "bam",
    "bwp",
    "bgn",
    "brl",
    "bnd",
    "khr",
    "cad",
    "kyd",
    "clp",
    "cny",
    "cop",
    "crc",
    "hrk",
    "cup",
    "czk",
    "dkk",
    "dop",
    "xcd",
    "egp",
    "svc",
    "eur",
    "fkp",
    "fjd",
    "ghs",
    "gip",
    "gtq",
    "ggp",
    "gyd",
    "hnl",
    "hkd",
    "huf",
    "isk",
    "inr",
    "idr",
    "irr",
    "imp",
    "ils",
    "jmd",
    "jpy",
    "jep",
    "kzt",
    "kpw",
    "krw",
    "kgs",
    "lak",
    "lbp",
    "lrd",
    "mkd",
    "myr",
    "mur",
    "mxn",
    "mnt",
    "mzn",
    "nad",
    "npr",
    "ang",
    "nzd",
    "nio",
    "ngn",
    "nok",
    "omr",
    "pkr",
    "pab",
    "pyg",
    "pen",
    "php",
    "pln",
    "qar",
    "ron",
    "rub",
    "shp",
    "sar",
    "rsd",
    "scr",
    "sgd",
    "sbd",
    "sos",
    "zar",
    "lkr",
    "sek",
    "chf",
    "srd",
    "syp",
    "twd",
    "thb",
    "ttd",
    "try",
    "tvd",
    "uah",
    "gbp",
    "usd",
    "uyu",
    "uzs",
    "vef",
    "vnd",
    "yer",
    "zwd"
   ],
   "description": "3-letter ISO code representing the currency. Lowercase."
  },
  "countryCode": {
   "type": "string",
   "minLength": 2,
   "maxLength": 2,
   "enum": [
    "af",
    "ax",
    "al",
    "dz",
    "as",
    "ad",
    "ao",
    "ai",
    "aq",
    "ag",
    "ar",
    "am",
    "aw",
    "au",
    "at",
    "az",
    "bs",
    "bh",
    "bd",
    "bb",
    "by",
    "be",
    "bz",
    "bj",
    "bm",
    "bt",
    "bo",
    "bq",
    "ba",
    "bw",
    "bv",
    "br",
    "io",
    "bn",
    "bg",
    "bf",
    "bi",
    "kh",
    "cm",
    "ca",
    "cv",
    "ky",
    "cf",
    "td",
    "cl",
    "cn",
    "cx",
    "cc",
    "co",
    "km",
    "cg",
    "cd",
    "ck",
    "cr",
    "ci",
    "hr",
    "cu",
    "cw",
    "cy",
    "cz",
    "dk",
    "dj",
    "dm",
    "do",
    "ec",
    "eg",
    "sv",
    "gq",
    "er",
    "ee",
    "et",
    "fk",
    "fo",
    "fj",
    "fi",
    "fr",
    "gf",
    "pf",
    "tf",
    "ga",
    "gm",
    "ge",
    "de",
    "gh",
    "gi",
    "gr",
    "gl",
    "gd",
    "gp",
    "gu",
    "gt",
    "gg",
    "gn",
    "gw",
    "gy",
    "ht",
    "hm",
    "va",
    "hn",
    "hk",
    "hu",
    "is",
    "in",
    "id",
    "ir",
    "iq",
    "ie",
    "im",
    "il",
    "it",
    "jm",
    "jp",
    "je",
    "jo",
    "kz",
    "ke",
    "ki",
    "kp",
    "kr",
    "kw",
    "kg",
    "la",
    "lv",
    "lb",
    "ls",
    "lr",
    "ly",
    "li",
    "lt",
    "lu",
    "mo",
    "mk",
    "mg",
    "mw",
    "my",
    "mv",
    "ml",
    "mt",
    "mh",
    "mq",
    "mr",
    "mu",
    "yt",
    "mx",
    "fm",
    "md",
    "mc",
    "mn",
    "me",
    "ms",
    "ma",
    "mz",
    "mm",
    "na",
    "nr",
    "np",
    "nl",
    "nc",
    "nz",
    "ni",
    "ne",
    "ng",
    "nu",
    "nf",
    "mp",
    "no",
    "om",
    "pk",
    "pw",
    "ps",
    "pa",
    "pg",
    "py",
    "pe",
    "ph",
    "pn",
    "pl",
    "pt",
    "pr",
    "qa",
    "re",
    "ro",
    "ru",
    "rw",
    "bl",
    "sh",
    "kn",
    "lc",
    "mf",
    "pm",
    "vc",
    "ws",
    "sm",
    "st",
    "sa",
    "sn",
    "rs",
    "sc",
    "sl",
    "sg",
    "sx",
    "sk",
    "si",
    "sb",
    "so",
    "za",
    "gs",
    "ss",
    "es",
    "lk",
    "sd",
    "sr",
    "sj",
    "sz",
    "se",
    "ch",
    "sy",
    "tw",
    "tj",
    "tz",
    "th",
    "tl",
    "tg",
    "tk",
    "to",
    "tt",
    "tn",
    "tr",
    "tm",
    "tc",
    "tv",
    "ug",
    "ua",
    "ae",
    "gb",
    "us",
    "um",
    "uy",
    "uz",
    "vu",
    "ve",
    "vn",
    "vg",
    "vi",
    "wf",
    "eh",
    "ye",
    "zm",
    "zw"
   ],
   "title": "ISO code representing the country",
   "description": "2-letter ISO code representing the country. United Kingdom is officially assigned the alpha-2 code gb rather than uk. Lowercase."
  }
 },
 "additionalProperties": false
}
"""
