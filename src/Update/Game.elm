module Update.Game exposing (update)

import Dict
import Game.Model exposing (Game, Pos, Point(Empty, Occupied), putPoint)
import Game.Util exposing (flipStone)
import Game.Msg


enterTile : Game -> Pos -> Game
enterTile game pos =
    { game | hovering = Just pos }


clickTile : Game -> Pos -> Game
clickTile game pos =
    let
        addStone =
            { game
                | board =
                    putPoint pos (Occupied game.nextPlayer) game.board
                , nextPlayer =
                    if game.handicap > 0 then
                        game.nextPlayer
                    else
                        flipStone game.nextPlayer
                , handicap =
                    if game.handicap > 0 then
                        game.handicap - 1
                    else
                        game.handicap
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
            Dict.get ( pos.x, pos.y ) game.board
    in
        Maybe.withDefault game <| Maybe.map updatePoint maybePoint


leaveBoard : Game -> Game
leaveBoard game =
    { game | hovering = Nothing }


update : Game.Msg.Msg -> Game -> Game
update msg game =
    case msg of
        Game.Msg.OnEnter pos ->
            enterTile game pos

        Game.Msg.OnLeave ->
            leaveBoard game

        Game.Msg.OnClick pos ->
            clickTile game pos
