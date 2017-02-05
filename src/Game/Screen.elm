module Game.Screen exposing (render)

import Game.Model
import Game.Util exposing (flipPlayer, playerToString)
import Html exposing (div, span, text)
import List.Zipper as Zipper


render : Game.Model.Game -> Html.Html msg
render game =
    let
        currentState =
            Zipper.current game.history
    in
        div []
            [ span [] [ text ("Current Player: " ++ playerToString currentState.nextPlayer) ]
            , span [] [ text ("Komi: " ++ toString game.komi) ]
            ]
