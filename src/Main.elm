module Main exposing (..)

import Platform exposing (Program)
import Maybe exposing (Maybe(..))
import Html exposing (map, beginnerProgram)
import Game.View exposing (renderBoard)
import Game.Model
import Game.Update
import Game.Msg
import Game.Screen
import Menu


type alias App =
    { setup : Menu.Setup
    , game : Game.Model.Game
    }


defaultSetup : Menu.DefaultSetup
defaultSetup =
    { size = Menu.Nineteen
    , komi = 6.5
    , okigo = 0
    }


createGame : Menu.Setup -> Maybe Game.Model.Game
createGame setup =
    Maybe.map2 (Game.Model.newGame (Menu.sizeToInt setup.size)) setup.komi setup.okigo


newApp : App
newApp =
    { setup = Menu.newSetup defaultSetup.size defaultSetup.komi defaultSetup.okigo
    , game = Game.Model.newGame (Menu.sizeToInt defaultSetup.size) defaultSetup.komi defaultSetup.okigo
    }


type Msg
    = GameUpdate Game.Msg.Msg
    | SetupUpdate Menu.Msg
    | ScreenUpdate Game.Screen.Msg


render : App -> Html.Html Msg
render app =
    Html.div []
        [ map GameUpdate <| renderBoard app.game
        , map SetupUpdate <| Menu.render defaultSetup app.setup
        , map ScreenUpdate <| Game.Screen.render app.game
        ]


update : Msg -> App -> App
update msg app =
    case msg of
        GameUpdate gameMsg ->
            { app | game = Game.Update.update gameMsg app.game }

        SetupUpdate menuMsg ->
            case menuMsg of
                Menu.Update setup ->
                    { app | setup = setup }

                Menu.NewGame ->
                    { app | game = Maybe.withDefault app.game (createGame app.setup) }

        ScreenUpdate screenMsg ->
            case screenMsg of
                Game.Screen.Undo ->
                    { app | game = Game.Model.undo app.game }

                Game.Screen.Redo ->
                    { app | game = Game.Model.redo app.game }


main : Program Never App Msg
main =
    beginnerProgram
        { model = newApp
        , view = render
        , update = update
        }
