module Main exposing (..)

import Platform exposing (Program)
import Html.App exposing (beginnerProgram)
import View
import Model
import Update


main : Program Never
main =
    beginnerProgram
        { model = Model.newApp
        , view = View.render
        , update = Update.update
        }
