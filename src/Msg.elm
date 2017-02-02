module Msg exposing (..)

import Msg.Game


type Msg
    = Start
    | Size String
    | Komi String
    | Handicap String
    | Game Msg.Game.Msg
