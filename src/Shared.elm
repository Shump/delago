module Shared exposing (..)

import Dict
import List
import Maybe exposing (Maybe)
import Maybe.Extra exposing (join, mapDefault)
import Array exposing (Array)

import Util exposing (cartesianProduct)

type Stone = Black | White

flipStone : Stone -> Stone
flipStone stone =
  case stone of
    Black -> White
    White -> Black

type Point = Empty | Occupied Stone

isOccupied : Point -> Bool
isOccupied point =
  case point of
    Empty -> False
    Occupied _ -> True

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
  , komi : Int
  , size : Int
  }

getPos : (Int, Int) -> Pos
getPos (x, y) = { x = x, y = y }

emptyBoard : Int -> Board
emptyBoard size =
  let
    positions = cartesianProduct [ 0 .. size - 1] [ 0 .. size - 1]
  in
    Dict.fromList <| List.map (\pos -> (pos, Empty)) positions

putPoint : Pos -> Point -> Board -> Board
putPoint {x, y} point board =
  Dict.update (x, y) (Maybe.map (\_ -> point)) board

newGame : Int -> Int -> (List Pos) -> Game
newGame size komi handicap =
    { board = emptyBoard size
    , nextPlayer = Black
    , hovering = Nothing
    , komi = komi
    , size = size
    }

type Msg
  = OnEnter Pos
  | OnLeave
  | OnClick Pos

