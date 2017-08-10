module Util exposing (foldResults, resultToDecoder)

import Json.Decode exposing (Decoder, succeed, fail)

foldResults : List (Result x y) -> Result x (List y)
foldResults results =
    results
        |> List.foldl
            (\t ->
                Result.andThen
                    (\r ->
                        Result.andThen (\x -> Ok <| x :: r) t
                    )
            )
            (Ok [])
        |> Result.andThen (Ok << List.reverse)


resultToDecoder : Result String a -> Decoder a
resultToDecoder res =
    case res of
        Ok a ->
            succeed a

        Err e ->
            fail e


