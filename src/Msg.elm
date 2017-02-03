module Msg exposing (..)

import Game.Msg


type Msg
    = Start
    | Size String
    | Komi String
    | Handicap String
    | Game Game.Msg.Msg
