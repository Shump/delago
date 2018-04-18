module Coding exposing (Coding(..), encode)

import Array
import Dict
import List
import Maybe
import String
import Tuple

import List.Zipper as Zipper

import Game.Types exposing (Game)

type Coding
    = List


listEncodingChars : Array.Array Char
listEncodingChars =
    String.toList "ABCDEFGHIJKLMNOPQRS"
        |> Array.fromList


listEncoding : Game -> String
listEncoding game =
    let
        currentBoard =
            Zipper.current game.history

        isBlack ( _, stone ) =
            stone == Game.Types.Black

        encodeCoor v =
            Array.get v listEncodingChars
                |> Maybe.withDefault 'x'

        encodePos ( x, y ) =
            ( String.fromChar <| encodeCoor x ) ++ ( String.fromChar <| encodeCoor y )

        encodedBoard =
            Dict.toList currentBoard
                |> List.partition isBlack
                |> uncurry List.append
                |> List.map (Tuple.first >> encodePos)
                |> String.concat

        encodedSize =
            String.padLeft 2 '0' <| toString game.size

        encodedHandicap =
            toString (min game.handicap 9)
    in
        encodedSize ++ encodedHandicap ++ encodedBoard


encode : Game -> Coding -> String
encode game coding =
    case coding of
        List ->
            listEncoding game
