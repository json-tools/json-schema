module Util exposing (foldResults, resultToDecoder, isInt, uncons, getAt, isUnique)

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


uncons : List a -> Maybe ( a, List a )
uncons l =
    case l of
        head :: tail ->
            Just ( head, tail )

        _ ->
            Nothing


getAt : Int -> List a -> Maybe a
getAt index =
    List.drop index >> List.head


isUnique : List comparable -> Bool
isUnique list =
    case list of
        head :: tail ->
            tail
                |> List.member head
                |> not
                |> (\x -> x && (isUnique tail))

        _ ->
            True
