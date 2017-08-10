module Tests exposing (all)


import Flat
import Test exposing (Test, describe)


all : Test
all =
    describe "Json.Schema"
        [ Flat.all
        ]
