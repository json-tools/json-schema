module Util exposing (foldResults, getAt, indexOfFirstDuplicate, isInt, isUnique, resultToDecoder, uncons)

import Json.Decode exposing (Decoder, fail, succeed)


foldResults : List (Result x y) -> Result x (List y)
foldResults results =
    results
        |> List.foldl
            (\t -> Result.andThen (\r -> t |> Result.map (\a -> a :: r)))
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
    indexOfFirstDuplicate list == -1


indexOfFirstDuplicate : List comparable -> Int
indexOfFirstDuplicate list =
    list
        |> List.foldl
            (\x ( index, res, sublist ) ->
                ( index + 1
                , if res > -1 then
                    res

                  else if List.member x sublist then
                    index

                  else
                    -1
                , sublist |> List.drop 1
                )
            )
            ( 0, -1, list |> List.drop 1 )
        |> (\( _, r, _ ) -> r)
