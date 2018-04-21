module Game.Board
    exposing
        ( Board
        , newBoard
        , putPoint
        , removePoint
        , getPoint
        , stonesCount
        , stones
        , blackStonesCount
        , blackStones
        , whiteStonesCount
        , whiteStones
        )

import Dict
import Maybe

import Maybe.Extra

import Game.Types exposing (..)


type alias Size = Int


type Board = Board (Dict.Dict ( Int, Int ) Stone)


newBoard : Size -> Board
newBoard _ =
    Board <| Dict.empty


putPoint : Pos -> Stone -> Board -> Board
putPoint { x, y } stone (Board board) =
    Board <| Dict.update ( x, y ) ( Maybe.Extra.or <| Just stone ) board


removePoint : Pos -> Board -> Board
removePoint { x, y } (Board board) =
    Board <| Dict.remove ( x, y ) board


getPoint : Board -> Pos -> Maybe Stone
getPoint (Board board) { x, y } =
    Dict.get ( x, y ) board


stonesCount : Board -> Int
stonesCount (Board board) =
    Dict.size board


stones : Board -> List ( ( Int, Int ), Stone )
stones (Board board) =
    Dict.toList board


blackStonesCount : Board -> Int
blackStonesCount (Board board) =
    Dict.foldl (\_ stone count -> if isBlack stone then count + 1 else count) 0 board


blackStones : Board -> List ( (Int, Int ), Stone )
blackStones (Board board) =
    Dict.foldl (\pos stone list -> if isBlack stone then ( pos, stone ) :: list else list) [] board


whiteStonesCount : Board -> Int
whiteStonesCount (Board board) =
    Dict.foldl (\_ stone count -> if isWhite stone then count + 1 else count) 0 board


whiteStones : Board -> List ( (Int, Int ), Stone )
whiteStones (Board board) =
    Dict.foldl (\pos stone list -> if isWhite stone then ( pos, stone ) :: list else list) [] board
