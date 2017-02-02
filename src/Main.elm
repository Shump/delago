module Main exposing (..)

import Platform exposing (Program)
import Html exposing (beginnerProgram)
import View
import Model
import Update
import Msg


main : Program Never Model.App Msg.Msg
main =
    beginnerProgram
        { model = Model.newApp
        , view = View.render
        , update = Update.update
        }
