module Msg exposing (..)

import Msg.Game

type Msg = Start | Game Msg.Game.Msg
