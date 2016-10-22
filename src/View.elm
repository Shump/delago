module View exposing (render)

import Html exposing (div, span, text, button)
import Html.App exposing (map)
import Html.Events exposing (onClick)

import Model
import Msg
import View.Board exposing (renderBoard)

render : Model.App -> Html.Html Msg.Msg
render app =
  case app of
    Model.NewGame setup ->
      div []
        [ text "New game"
        , button [ onClick Msg.Start ] [ text "Start!" ]
        ]
    Model.Game game -> map (\msg -> Msg.Game msg) <| renderBoard game
