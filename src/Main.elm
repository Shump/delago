module Main exposing (..)

import Platform exposing (Program)
import Html exposing (map, beginnerProgram)
import Game.View exposing (renderBoard)
import Game.Model
import Game.Msg
import Menu


type App
    = NewGame Menu.Setup
    | Game Game.Model.Game


newApp : App
newApp =
    NewGame Menu.newSetup


type Msg
    = GameUpdate Game.Msg.Msg
    | SetupUpdate Menu.Msg


render : App -> Html.Html Msg
render app =
    case app of
        NewGame setup ->
            map (\msg -> SetupUpdate msg) <| Menu.render setup

        Game game ->
            map (\msg -> GameUpdate msg) <| renderBoard game


main : Program Never App Msg
main =
    beginnerProgram
        { model = newApp
        , view = render
        , update = (\msg app -> app)
        }
