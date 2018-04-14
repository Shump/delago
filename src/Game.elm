module Game exposing (..)

import Dict
import List
import Maybe exposing (Maybe)
import Util exposing (cartesianProduct)

import List.Zipper as Zipper

import Game.Types exposing (..)
import Game.Util exposing (flipStone, flipPlayer)
import List.Zipper.Extra exposing (previous_, next_, replaceRight, next_)


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
        { history = Zipper.singleton <| BoardState (emptyBoard size) Black handicap
        , hovering = Nothing
        , komi = komi
        , size = size
        }


undo : Game -> Game
undo game =
    { game | history = previous_ game.history }


redo : Game -> Game
redo game =
    { game | history = next_ game.history }


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
