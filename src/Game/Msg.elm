module Game.Msg exposing (..)

import Game.Model


type Msg
    = OnEnter Game.Model.Pos
    | OnLeave
    | OnClick Game.Model.Pos
