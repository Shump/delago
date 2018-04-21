module Game exposing (..)

import Maybe exposing (Maybe)

import List.Zipper as Zipper

import Game.Board
import Game.Game exposing (Game)
import Game.Types exposing (..)
import Game.Util exposing (flipStone, flipPlayer)
import List.Zipper.Extra exposing (previous_, next_, replaceRight, next_)


newGame : Int -> Int -> Game
newGame size handicap =
    let
        emptyBoard size =
            Game.Board.newBoard size
    in
        { history = Zipper.singleton <| emptyBoard size
        , hovering = Nothing
        , size = size
        , handicap = handicap
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
        currentBoard =
            Zipper.current game.history

        nextPlayer =
            Game.Util.nextPlayer currentBoard game.handicap

        addStone =
            let
                newBoard =
                    Game.Board.putPoint pos nextPlayer currentBoard
            in
                { game | history = next_ <| replaceRight [ newBoard ] game.history }

        removeStone =
            let
                newBoard =
                    Game.Board.removePoint pos currentBoard
            in
                { game | history = next_ <| replaceRight [ newBoard ] game.history }

        updatePoint point =
            case point of
                Nothing ->
                    addStone

                Just _ ->
                    removeStone

        maybePoint =
            Game.Board.getPoint currentBoard pos
    in
        updatePoint maybePoint


leaveBoard : Game -> Game
leaveBoard game =
    { game | hovering = Nothing }
