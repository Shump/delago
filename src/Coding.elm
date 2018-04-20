module Coding exposing (Coding(..), encode, decode)

import Array
import Dict
import List
import Maybe
import String
import Tuple

import List.Extra
import List.Zipper as Zipper
import Maybe.Extra

import Game.Types exposing (Game)
import Util

type Coding
    = List


type alias Pos =
    ( Int, Int )


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


listDecoding : String -> Maybe.Maybe Game
listDecoding str =
    let
        sizeString =
            String.left 2 str

        handicapString =
            String.slice 2 3 str

        boardString =
            String.dropLeft 3 str

        size =
            String.toInt sizeString
                |> Result.toMaybe

        handicap =
            String.toInt handicapString
                |> Result.toMaybe

        lut : Dict.Dict Char Int
        lut =
            Array.toIndexedList listEncodingChars
                |> List.map (\( i, c ) -> ( c, i ))
                |> Dict.fromList

        charToCoor c =
            Dict.get c lut

        split : Int -> Int -> List Pos -> ( List Pos, List Pos )
        split size handicap positions =
            let
                stones =
                    List.length positions

                whiteStones =
                    (stones - handicap) // 2

                blackStones =
                    stones - whiteStones
            in
                List.Extra.splitAt blackStones positions

        toBoard : Int -> Int -> ( List Pos, List Pos ) -> Game.Types.Board
        toBoard handicap size (bs, ws) =
            let
                insert stone pos board =
                    Dict.insert pos stone board

                blackAdded =
                    List.foldl (insert Game.Types.Black) Dict.empty bs
            in
                List.foldl (insert Game.Types.White) blackAdded ws

        board : Maybe Game.Types.Board
        board =
            String.toList boardString
                |> Maybe.Extra.traverse charToCoor
                |> Maybe.map Util.pairs
                |> Maybe.map3 split size handicap
                |> Maybe.map3 toBoard handicap size

        game : Float -> Int -> Int -> Game.Types.Board -> Game.Types.Game
        game komi size handicap board =
            { history = Zipper.singleton board
            , hovering = Maybe.Nothing
            , komi = komi
            , size = size
            , handicap = handicap
            }
    in
        Maybe.map3 (game 4.5) size handicap board


decode : Coding -> String -> Maybe Game
decode coding str =
    case coding of
        List ->
            listDecoding str
