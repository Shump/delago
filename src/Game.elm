module Game exposing (..)

import Dict
import Maybe exposing (Maybe)

import List.Zipper as Zipper
import Maybe.Extra

import Game.Types exposing (..)
import Game.Util exposing (flipStone, flipPlayer)
import List.Zipper.Extra exposing (previous_, next_, replaceRight, next_)


putPoint : Pos -> Stone -> Board -> Board
putPoint { x, y } stone board =
    Dict.update ( x, y ) ( Maybe.Extra.or <| Just stone ) board


newGame : Int -> Float -> Int -> Game
newGame size komi handicap =
    let
        emptyBoard _ =
            Dict.empty
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
                    putPoint pos (currentState.nextPlayer) currentState.board

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
                    { currentState | board = Dict.remove ( pos.x, pos.y ) currentState.board }
            in
                { game
                    | history = next_ <| replaceRight [ newState ] game.history
                }

        updatePoint point =
            case point of
                Nothing ->
                    addStone

                Just _ ->
                    removeStone

        maybePoint =
            Dict.get ( pos.x, pos.y ) currentState.board
    in
        updatePoint maybePoint


leaveBoard : Game -> Game
leaveBoard game =
    { game | hovering = Nothing }
