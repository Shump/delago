module Update exposing (update)

import Model
import Update.Game
import Msg

update : Msg.Msg -> Model.App -> Model.App
update msg app =
  case msg of
    Msg.Start -> Model.start app
    Msg.Game msg_ ->
      Model.updateGame (Update.Game.update msg_) app
