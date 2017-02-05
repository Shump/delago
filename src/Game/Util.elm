module Game.Util exposing (..)

import Game.Model exposing (..)


stoneToString : Stone -> String
stoneToString stone =
    case stone of
        Black ->
            "Black"

        White ->
            "White"


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


playerToString : Player -> String
playerToString =
    stoneToString


isOccupied : Point -> Bool
isOccupied point =
    case point of
        Empty ->
            False

        Occupied _ ->
            True


getPos : ( Int, Int ) -> Pos
getPos ( x, y ) =
    { x = x, y = y }
