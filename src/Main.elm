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


defaultSetup : Menu.Setup
defaultSetup =
    Menu.newSetup Menu.Nineteen 6.5 0


createGame : Menu.Setup -> Game.Model.Game
createGame setup =
    Game.Model.newGame (Menu.sizeToInt setup.size) setup.komi setup.okigo


newApp : App
newApp =
    { setup = defaultSetup
    , game = createGame defaultSetup
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
                    { app | game = createGame app.setup }


main : Program Never App Msg
main =
    beginnerProgram
        { model = newApp
        , view = render
        , update = update
        }
