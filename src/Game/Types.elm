module Game.Types exposing (..)

import Dict

import List.Zipper as Zipper


type Stone
    = Black
    | White


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Board =
    Dict.Dict ( Int, Int ) Stone


type alias Player =
    Stone


type alias BoardState =
    { board : Board
    }


type alias Game =
    { history : Zipper.Zipper BoardState
    , hovering : Maybe Pos
    , komi : Float
    , size : Int
    , handicap : Int
    }
