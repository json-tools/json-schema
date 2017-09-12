module Util exposing (foldResults, resultToDecoder, isInt)

import Json.Decode exposing (Decoder, succeed, fail)


foldResults : List (Result x y) -> Result x (List y)
foldResults results =
    results
        |> List.foldl
            (\t -> Result.andThen (\r -> t |> Result.map (flip (::) r)))
            (Ok [])
        |> Result.map List.reverse


resultToDecoder : Result String a -> Decoder a
resultToDecoder res =
    case res of
        Ok a ->
            succeed a

        Err e ->
            fail e


isInt : Float -> Bool
isInt x =
    x == (round >> toFloat) x
