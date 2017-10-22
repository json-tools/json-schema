module Tests exposing (all)

import Decoding
import Validations
import Schema
import DataManipulation
import Generate
import Test exposing (Test, describe)


all : Test
all =
    describe "Json.Schema"
        [ Decoding.all
        , Validations.all
        , Schema.all
        , DataManipulation.all
        , Generate.all
        ]
