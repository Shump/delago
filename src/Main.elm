module Main exposing (..)

import Platform exposing (Program)
import Html exposing (map, beginnerProgram)
import Game.View exposing (renderBoard)
import Game.Model
import Game.Update
import Game.Msg
import Menu


type alias App =
    { setup : Menu.Setup
    , game : Game.Model.Game
    }


newApp : App
newApp =
    { setup = Menu.newSetup
    , game = Game.Model.newGame 19 0 0
    }


type Msg
    = GameUpdate Game.Msg.Msg
    | SetupUpdate Menu.Msg


render : App -> Html.Html Msg
render app =
    Html.div []
        [ map (\msg -> SetupUpdate msg) <| Menu.render app.setup
        , map (\msg -> GameUpdate msg) <| renderBoard app.game
        ]


update : Msg -> App -> App
update msg app =
    case msg of
        GameUpdate gameMsg ->
            { app | game = Game.Update.update gameMsg app.game }

        SetupUpdate setupMsg ->
            app


main : Program Never App Msg
main =
    beginnerProgram
        { model = newApp
        , view = render
        , update = update
        }
