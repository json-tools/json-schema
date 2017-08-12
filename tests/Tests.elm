module Tests exposing (all)


import Flat
import Validation
import Test exposing (Test, describe)


all : Test
all =
    describe "Json.Schema"
        [ Flat.all
        , Validation.all
        ]
