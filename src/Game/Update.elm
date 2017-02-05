module Game.Update exposing (update)

import Dict
import Game.Model exposing (Game, Pos, Point(Empty, Occupied), putPoint)
import Game.Util exposing (flipStone, flipPlayer)
import Game.Msg
import List.Zipper as Zipper
import List.Zipper.Extra exposing (replaceRight, next_)


enterTile : Game -> Pos -> Game
enterTile game pos =
    { game | hovering = Just pos }


clickTile : Game -> Pos -> Game
clickTile game pos =
    let
        currentState =
            Zipper.current game.history

        addStone =
            let
                newBoard =
                    putPoint pos (Occupied currentState.nextPlayer) currentState.board

                newState =
                    { currentState
                        | board = newBoard
                        , nextPlayer =
                            if currentState.handicap > 0 then
                                currentState.nextPlayer
                            else
                                flipPlayer currentState.nextPlayer
                        , handicap =
                            if currentState.handicap > 0 then
                                currentState.handicap - 1
                            else
                                currentState.handicap
                    }
            in
                { game | history = next_ <| replaceRight [ newState ] game.history }

        removeStone =
            let
                newState =
                    { currentState | board = putPoint pos Empty currentState.board }
            in
                { game
                    | history = next_ <| replaceRight [ newState ] game.history
                }

        updatePoint point =
            case point of
                Empty ->
                    addStone

                Occupied _ ->
                    removeStone

        maybePoint =
            Dict.get ( pos.x, pos.y ) currentState.board
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
