module Tests exposing (all)


import Flat
import Validations
import Schema
import Test exposing (Test, describe)


all : Test
all =
    describe "Json.Schema"
        [ Flat.all
        , Validations.all
        , Schema.all
        ]
