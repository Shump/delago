module Digits exposing (..)

import String
import List
import List.Extra exposing (elemIndex, unfoldr, getAt)
import Maybe.Extra exposing (traverse, combine)


type Digits
    = Digits Int (List Int)


toString : String -> Digits -> String
toString alpha digits =
    let
        alpha_ =
            String.toList alpha

        base =
            String.length alpha

        (Digits _ digs) =
            toBase base digits

        chars =
            List.map (\d -> getAt d alpha_) digs
    in
        Maybe.withDefault "" <| Maybe.map String.fromList <| combine chars


toInt : Digits -> Int
toInt (Digits base digs) =
    let
        acc dig ( pow, val ) =
            ( pow + 1, val + (base ^ pow) * dig )
    in
        Tuple.second <| List.foldr acc ( 0, 0 ) digs


toBin : Digits -> String
toBin digits =
    toString "01" <| toBase 2 digits


toHex : Digits -> String
toHex digits =
    toString "0123456789ABCDEF" <| toBase 16 digits


toBase_ : Int -> Int -> Digits
toBase_ base num =
    let
        f n =
            case n of
                0 ->
                    Nothing

                _ ->
                    Just ( rem n base, n // base )
    in
        Digits base <| List.reverse <| unfoldr f num


toBase : Int -> Digits -> Digits
toBase base digits =
    toBase_ base (toInt digits)


fromString : String -> Int -> String -> Maybe Digits
fromString alphabet base string =
    let
        alpha =
            String.toList alphabet

        chars =
            String.toList string

        f c =
            elemIndex c alpha

        encoded =
            traverse f chars
    in
        case encoded of
            Nothing ->
                Nothing

            Just digits ->
                Just <| Digits base digits


fromInt : Int -> Digits
fromInt num =
    toBase_ 10 num


fromHex : String -> Maybe Digits
fromHex string =
    fromString "0123456789ABCDEF" 16 <| String.toUpper string


fromBin : String -> Maybe Digits
fromBin string =
    fromString "01" 2 string
