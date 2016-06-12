module Main exposing (..)

import Debug
import Dict
import Maybe exposing (Maybe)

import Platform exposing (Program)
import Html
import Html.App exposing (beginnerProgram)

import View
import Shared

view : Shared.Game -> Html.Html Shared.Msg
view game =
  let
    viewCallbacks =
      { onEnter = Shared.OnEnter
      , onLeave = Shared.OnLeave
      , onClick = Shared.OnClick
      }
  in
    View.render viewCallbacks game

update : Shared.Msg -> Shared.Game -> Shared.Game
update msg game =
  let
    model =
      case msg of
        Shared.OnEnter pos -> { game | hovering = Just pos }
        Shared.OnLeave -> { game | hovering = Nothing }
        Shared.OnClick pos ->
          let
            addStone =
              { game
              | board =
                  Shared.putPoint pos (Shared.Occupied game.nextPlayer) game.board
              , nextPlayer =
                  Shared.flipStone game.nextPlayer
              }

            removeStone =
              { game
              | board =
                  Shared.putPoint pos Shared.Empty game.board
              }

            updatePoint point =
              case point of
                Shared.Empty ->
                  addStone
                Shared.Occupied _ ->
                  removeStone

            maybePoint =
              Dict.get (pos.x, pos.y) game.board

          in
            Maybe.withDefault game <| Maybe.map updatePoint maybePoint
  in
    model

main : Program Never
main =
    beginnerProgram
      { model = Shared.newGame 19 0 []
      , view = view
      , update = update
      }
