module Model.Util exposing (..)

import Model.Game exposing (..)

flipStone : Stone -> Stone
flipStone stone =
  case stone of
    Black -> White
    White -> Black

isOccupied : Point -> Bool
isOccupied point =
  case point of
    Empty -> False
    Occupied _ -> True

getPos : (Int, Int) -> Pos
getPos (x, y) = { x = x, y = y }
