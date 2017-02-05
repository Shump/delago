module Main exposing (..)

import Platform exposing (Program)
import Maybe exposing (Maybe(..))
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


render : App -> Html.Html Msg
render app =
    Html.div []
        [ map (\msg -> SetupUpdate msg) <|
            Menu.render defaultSetup app.setup
        , map (\msg -> GameUpdate msg) <| renderBoard app.game
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


main : Program Never App Msg
main =
    beginnerProgram
        { model = newApp
        , view = render
        , update = update
        }
