module Game.Util exposing (..)

import Dict

import Game.Types exposing (..)


flipStone : Stone -> Stone
flipStone stone =
    case stone of
        Black ->
            White

        White ->
            Black


flipPlayer : Player -> Player
flipPlayer =
    flipStone


stoneToString : Stone -> String
stoneToString stone =
    case stone of
        Black ->
            "Black"

        White ->
            "White"


playerToString : Player -> String
playerToString =
    stoneToString


nextPlayer : Board -> Int -> Player
nextPlayer board handicap  =
    let
        diff = (stonesCount board) - handicap
    in
        if (rem (max diff 0) 2) == 0 then
            Black
        else
            White


getPos : ( Int, Int ) -> Pos
getPos ( x, y ) =
    { x = x, y = y }
