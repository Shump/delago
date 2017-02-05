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


isCycleOf : Float -> Float -> Bool
isCycleOf value cycle =
    let
        cycles =
            value / cycle
    in
        cycles == toFloat (truncate cycles)
