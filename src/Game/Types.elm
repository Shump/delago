module Game.Types
    exposing
        (Size
        , Stone(..)
        , Pos
        , Board
        , Player
        , Game
        , newBoard
        , putPoint
        , removePoint
        , getPoint
        , stonesCount
        , stones
        )

import Dict
import Maybe

import List.Zipper as Zipper
import Maybe.Extra


type alias Size = Int


type Stone
    = Black
    | White


type alias Pos =
    { x : Int
    , y : Int
    }


type Board = Board (Dict.Dict ( Int, Int ) Stone)


type alias Player =
    Stone


type alias Game =
    { history : Zipper.Zipper Board
    , hovering : Maybe Pos
    , size : Size
    , handicap : Int
    }


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
