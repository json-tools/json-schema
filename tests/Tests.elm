module Tests exposing (all)


import Type
import Test exposing (Test, describe)


all : Test
all =
    describe "Json.Schema"
        [ Type.all
        ]
