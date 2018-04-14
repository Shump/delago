module Game.Types exposing (..)

import Dict

import List.Zipper as Zipper


type Stone
    = Black
    | White


type Point
    = Empty
    | Occupied Stone


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Board =
    Dict.Dict ( Int, Int ) Point


type alias Player =
    Stone


type alias BoardState =
    { board : Board
    , nextPlayer : Player
    , handicap : Int
    }


type alias Game =
    { history : Zipper.Zipper BoardState
    , hovering : Maybe Pos
    , komi : Float
    , size : Int
    }
