module Game.Model exposing (..)

import Dict
import List
import Maybe exposing (Maybe)
import Util exposing (cartesianProduct)
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
    }


type alias Game =
    { history : Zipper.Zipper BoardState
    , hovering : Maybe Pos
    , komi : Float
    , handicap : Int
    , size : Int
    }


putPoint : Pos -> Point -> Board -> Board
putPoint { x, y } point board =
    Dict.update ( x, y ) (Maybe.map (\_ -> point)) board


newGame : Int -> Float -> Int -> Game
newGame size komi handicap =
    let
        emptyBoard size =
            let
                positions =
                    cartesianProduct (List.range 0 (size - 1)) (List.range 0 (size - 1))
            in
                Dict.fromList <| List.map (\pos -> ( pos, Empty )) positions
    in
        { history = Zipper.singleton <| BoardState (emptyBoard size) Black
        , hovering = Nothing
        , komi = komi
        , handicap = handicap
        , size = size
        }
