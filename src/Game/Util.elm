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


stones : Board -> Int
stones board = Dict.size board




getPos : ( Int, Int ) -> Pos
getPos ( x, y ) =
    { x = x, y = y }
