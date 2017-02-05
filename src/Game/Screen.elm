module Game.Screen exposing (render)

import Game.Model
import Game.Util exposing (flipPlayer, playerToString)
import Html exposing (div, span, text)


render : Game.Model.Game -> Html.Html msg
render game =
    div []
        [ span [] [ text ("Current Player: " ++ playerToString game.nextPlayer) ]
        , span [] [ text ("Komi: " ++ toString game.komi) ]
        ]
