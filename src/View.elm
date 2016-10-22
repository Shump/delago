module View exposing (render)

import Html exposing (div, span, text)

import Model

import View.Shared
import View.Board exposing (renderBoard)

render : View.Shared.Callbacks msg -> Model.Game -> Html.Html msg
render callbacks game =
  div []
    [ renderBoard callbacks game
    ]
