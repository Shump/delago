module Msg.Game exposing (..)

import Model.Game

type Msg
  = OnEnter Model.Game.Pos
  | OnLeave
  | OnClick Model.Game.Pos
