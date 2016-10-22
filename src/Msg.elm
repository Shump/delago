module Msg exposing (..)

import Model

type Msg
  = OnEnter Model.Pos
  | OnLeave
  | OnClick Model.Pos
