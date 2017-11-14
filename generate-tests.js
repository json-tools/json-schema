const { readdirSync, readFileSync } = require('fs');
const { join } = require('path');

const tests = load('./JSON-Schema-Test-Suite/tests/draft6');
console.log(header('Draft6') + body('draft-6', tests) + footer());

function load(path) {
    return readdirSync(path)
        .filter(x => x.endsWith('.json'))
        .filter(x => x !== 'refRemote.json')
        .map(filename => {
            return { filename, suite: JSON.parse(readFileSync(join(path, filename))) };
        });
}

function header(moduleName) {
    return `module ${moduleName} exposing (all)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (decodeString, value)
import Json.Schema.Definitions exposing (blankSchema, decoder)
import Json.Schema exposing (validateValue)
import Test exposing (Test, describe, test, only)
import Expect


all : Test
all =
`;

}

function footer() {
    return `


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
            validateValue data schema
                |> Result.mapError (\\_ -> "")
                |> Result.map (\\_ -> True)
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
`
}

function body(name, tests) {
    return `    describe "${name}"
        [ ` +
    tests.map(({filename, suite}) => {
        return `describe "${filename}"
             [ ${printSuite(suite).join('\n        , ')} ]`
    }).join('\n            , ')
    + ']';
}

function printSuite(cases) {
    return cases.map(({description, schema, tests}) => {
        return `describe "suite: ${description}"
                [ ${printCases(schema, tests).join('\n                , ')}
                ]
            `
        ;
    });
}


function printCases(schema, collection) {
    return collection.map(({description, data, valid}) => {
        return `test "${description}" <|
                    \\() ->
                        examine
                            """
                            ${JSON.stringify(schema)}
                            """
                            """
                            ${JSON.stringify(data)}
                            """
                            ${valid ? 'True' : 'False'}`
    });
}
