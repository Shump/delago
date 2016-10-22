module Update exposing (update)

import Dict

import Model.Game exposing (Game, Pos, Point(Empty, Occupied), putPoint)
import Model.Util exposing (flipStone)
import Msg

enterTile : Game -> Pos -> Game
enterTile game pos = { game | hovering = Just pos }

clickTile : Game -> Pos -> Game
clickTile game pos =
  let
    addStone =
      { game
      | board =
          putPoint pos (Occupied game.nextPlayer) game.board
      , nextPlayer =
          Model.Util.flipStone game.nextPlayer
      }

    removeStone =
      { game
      | board =
          putPoint pos Empty game.board
      }

    updatePoint point =
      case point of
        Empty ->
          addStone
        Occupied _ ->
          removeStone

    maybePoint =
      Dict.get (pos.x, pos.y) game.board

  in
    Maybe.withDefault game <| Maybe.map updatePoint maybePoint

leaveBoard : Game -> Game
leaveBoard game = { game | hovering = Nothing }

update : Msg.Msg -> Game -> Game
update msg game =
  case msg of
    Msg.OnEnter pos -> enterTile game pos
    Msg.OnLeave -> leaveBoard game
    Msg.OnClick pos -> clickTile game pos
