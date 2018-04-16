module Main exposing (..)

import Html exposing (beginnerProgram, div, span, text, button)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import List
import Maybe exposing (Maybe(..))
import Platform exposing (Program)

import List.Zipper as Zipper

import Game
import Game.Types
import Game.Util exposing (flipPlayer, playerToString)
import Game.View exposing (renderBoard)
import Menu


type alias App =
    { setup : Menu.Setup
    , game : Game.Types.Game
    }


defaultSetup : Menu.DefaultSetup
defaultSetup =
    { size = Menu.Nineteen
    , komi = 6.5
    , okigo = 0
    }


createGame : Menu.Setup -> Maybe Game.Types.Game
createGame setup =
    Maybe.map2 (Game.newGame (Menu.sizeToInt setup.size)) setup.komi setup.okigo


newApp : App
newApp =
    { setup = Menu.newSetup defaultSetup.size defaultSetup.komi defaultSetup.okigo
    , game = Game.newGame (Menu.sizeToInt defaultSetup.size) defaultSetup.komi defaultSetup.okigo
    }


type Msg
    = OnEnter Game.Types.Pos
    | OnLeave
    | OnClick Game.Types.Pos
    | Undo
    | Redo
    | UpdateSetup Menu.Setup
    | NewGame


renderScreen : Game.Types.Game -> Html.Html Msg
renderScreen game =
    let
        currentBoard =
            Zipper.current game.history

        nextPlayer =
            Game.Util.nextPlayer currentBoard game.handicap
    in
        div []
            [ span [] [ text ("Current Player: " ++ playerToString nextPlayer) ]
            , span [] [ text ("Komi: " ++ toString game.komi) ]
            , div []
                [ button
                    [ onClick Undo
                    , disabled (List.isEmpty <| Zipper.before game.history)
                    ]
                    [ text "Undo" ]
                , button
                    [ onClick Redo
                    , disabled (List.isEmpty <| Zipper.after game.history)
                    ]
                    [ text "Redo" ]
                ]
            ]


render : App -> Html.Html Msg
render app =
    Html.div []
        [ renderBoard app.game
            { onEnter = OnEnter
            , onLeave = OnLeave
            , onClick = OnClick
            }
        , Menu.render defaultSetup app.setup
            { update = UpdateSetup
            , newGame = NewGame
            }
        , renderScreen app.game
        ]


update : Msg -> App -> App
update msg app =
    case msg of
        OnEnter pos ->
            { app | game = Game.enterTile app.game pos }

        OnLeave ->
            { app | game = Game.leaveBoard app.game }

        OnClick pos ->
            { app | game = Game.clickTile app.game pos }

        Undo ->
            { app | game = Game.undo app.game }

        Redo ->
            { app | game = Game.redo app.game }

        UpdateSetup setup ->
            { app | setup = setup }

        NewGame ->
            { app | game = Maybe.withDefault app.game (createGame app.setup) }


main : Program Never App Msg
main =
    beginnerProgram
        { model = newApp
        , view = render
        , update = update
        }
