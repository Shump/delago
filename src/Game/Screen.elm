module Game.Screen exposing (Msg(..), render)

import Html exposing (div, span, text, button)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import List
import List.Zipper as Zipper
import Game.Model
import Game.Util exposing (flipPlayer, playerToString)


type Msg
    = Undo
    | Redo


render : Game.Model.Game -> Html.Html Msg
render game =
    let
        currentState =
            Zipper.current game.history
    in
        div []
            [ span [] [ text ("Current Player: " ++ playerToString currentState.nextPlayer) ]
            , span [] [ text ("Komi: " ++ toString game.komi) ]
            , div []
                [ button
                    [ onClick Undo
                    , disabled (List.isEmpty <| Zipper.before game.history)
                    ]
                    [ text "Undo" ]
                , button
                    [ onClick Redo
                    , disabled (List.isEmpty <| Zipper.after game.history)
                    ]
                    [ text "Redo" ]
                ]
            ]
