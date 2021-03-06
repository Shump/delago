module Util exposing (..)

import List

import List.Extra exposing (andThen)


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct xs ys =
    xs
        |> andThen
            (\x ->
                ys
                    |> andThen
                        (\y ->
                            [ ( x, y ) ]
                        )
            )



-- Name borrowed from https://github.com/danielnarey/elm-toolkit


applyList : List (a -> b) -> a -> List b
applyList fs v =
    List.map (\f -> f v) fs


all : List Bool -> Bool
all bs =
    List.foldr (&&) True bs


any : List Bool -> Bool
any bs =
    List.foldr (||) False bs


pairs : List a -> List ( a, a )
pairs xs =
    let
        maybeFirst = List.head xs

        tail = List.drop 1 xs

        maybeSecond = List.head tail
    in
        case ( maybeFirst, maybeSecond ) of
            ( Just a, Just b ) ->
                ( a, b ) :: (pairs <| List.drop 1 tail)

            ( _, _ ) ->
                []
