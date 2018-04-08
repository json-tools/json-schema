## [not published]

## 4.0.0

- support full spec of draft-04 and draft-06
- functionality validated by [official test suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite)
- dozen of fixes in validation
- new type `ExclusiveBoundary(BoolBoundary, NumberBoundary)` (compatibility layer between drafts 4 and 6)

## 3.0.0

- changed validation errors (added more info to facilitate error messages building)

## 2.0.0

Multiple errors with details returned by validation:
- added `Json.Schema.Validation`
- changed validation output format from `Result String Bool` to `Result (List Json.Schema.Validation.Error) Value`

## 1.1.0

Added `Json.Schema.Random` API to generate random values


## 1.0.0

Initial capabilities: decode/encode, validate with a single string error
