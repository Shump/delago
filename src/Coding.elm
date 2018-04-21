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

import Game.Board
import Game.Game exposing (Game)
import Game.Types
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
            Game.Board.stones currentBoard
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

        toBoard : Int -> Int -> ( List Pos, List Pos ) -> Game.Board.Board
        toBoard handicap size (bs, ws) =
            let
                insert stone ( x, y ) board =
                    Game.Board.putPoint (Game.Types.Pos x y) stone board

                blackAdded =
                    List.foldl (insert Game.Types.Black) (Game.Board.newBoard size) bs
            in
                List.foldl (insert Game.Types.White) blackAdded ws

        board : Maybe Game.Board.Board
        board =
            String.toList boardString
                |> Maybe.Extra.traverse charToCoor
                |> Maybe.map Util.pairs
                |> Maybe.map3 split size handicap
                |> Maybe.map3 toBoard handicap size

        game : Int -> Int -> Game.Board.Board -> Game
        game size handicap board =
            { history = Zipper.singleton board
            , hovering = Maybe.Nothing
            , size = size
            , handicap = handicap
            }
    in
        Maybe.map3 game size handicap board


decode : Coding -> String -> Maybe Game
decode coding str =
    case coding of
        List ->
            listDecoding str
