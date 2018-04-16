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
        { history = Zipper.singleton <| emptyBoard size
        , hovering = Nothing
        , komi = komi
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
                    putPoint pos nextPlayer currentBoard
            in
                { game | history = next_ <| replaceRight [ newBoard ] game.history }

        removeStone =
            let
                newBoard =
                    Dict.remove ( pos.x, pos.y ) currentBoard
            in
                { game | history = next_ <| replaceRight [ newBoard ] game.history }

        updatePoint point =
            case point of
                Nothing ->
                    addStone

                Just _ ->
                    removeStone

        maybePoint =
            Dict.get ( pos.x, pos.y ) currentBoard
    in
        updatePoint maybePoint


leaveBoard : Game -> Game
leaveBoard game =
    { game | hovering = Nothing }
