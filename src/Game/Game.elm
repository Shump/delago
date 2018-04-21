module Game.Game exposing (Game)

import List.Zipper as Zipper

import Game.Board
import Game.Types


type alias Game =
    { history : Zipper.Zipper Game.Board.Board
    , hovering : Maybe Game.Types.Pos
    , size : Int
    , handicap : Int
    }
