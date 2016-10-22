module Main exposing (..)

import Platform exposing (Program)
import Html.App exposing (beginnerProgram)

import View
import View.Shared exposing (Callbacks)
import Model
import Update
import Msg

viewCallbacks : Callbacks Msg.Msg
viewCallbacks =
  { onEnter = Msg.OnEnter
  , onLeave = Msg.OnLeave
  , onClick = Msg.OnClick
  }

main : Program Never
main =
    beginnerProgram
      { model = Model.newGame 19 0 0
      , view = View.render viewCallbacks
      , update = Update.update
      }
