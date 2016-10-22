module Model exposing (..)

import Dict
import List
import Maybe exposing (Maybe)

import Util exposing (cartesianProduct)

type Stone = Black | White

type Point = Empty | Occupied Stone

type alias Pos =
  { x : Int
  , y : Int
  }

type alias Board = Dict.Dict (Int, Int) Point

type alias Player = Stone

type alias Game =
  { board : Board
  , nextPlayer : Player
  , hovering : Maybe Pos
  , komi : Float
  , size : Int
  }

putPoint : Pos -> Point -> Board -> Board
putPoint {x, y} point board =
  Dict.update (x, y) (Maybe.map (\_ -> point)) board

newGame : Int -> Float -> Int -> Game
newGame size komi handicap =
  let
    emptyBoard size =
      let
        positions = cartesianProduct [ 0 .. size - 1] [ 0 .. size - 1]
      in
        Dict.fromList <| List.map (\pos -> (pos, Empty)) positions
  in
    { board = emptyBoard size
    , nextPlayer = Black
    , hovering = Nothing
    , komi = komi
    , size = size
    }
