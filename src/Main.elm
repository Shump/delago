module Main exposing (..)

import Html exposing (beginnerProgram, div, span, text, button)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Navigation
import List
import Maybe exposing (Maybe(..))
import Platform exposing (Program)

import List.Zipper as Zipper
import Url
import UrlParser

import Coding
import Game
import Game.Game exposing (Game)
import Game.Types
import Game.Util exposing (flipPlayer, playerToString)
import Game.View exposing (renderBoard)
import Menu


type alias App =
    { setup : Menu.Setup
    , game : Game
    }


defaultSetup : Menu.DefaultSetup
defaultSetup =
    { size = Menu.Nineteen
    , okigo = 0
    }


createGame : Menu.Setup -> Maybe Game
createGame setup =
    Maybe.map (Game.newGame (Menu.sizeToInt setup.size)) setup.okigo


newApp : Navigation.Location -> ( App, Cmd Msg )
newApp location =
    let
        newGame =
            Game.newGame (Menu.sizeToInt defaultSetup.size) defaultSetup.okigo

        game =
            UrlParser.parseHash UrlParser.string location
                |> Maybe.andThen (Coding.decode Coding.List)
                |> Maybe.withDefault newGame

        app =
            { setup = Menu.newSetup defaultSetup.size defaultSetup.okigo
            , game = game
            }
    in
        ( app, Cmd.none )


type Msg
    = OnEnter Game.Types.Pos
    | OnLeave
    | OnClick Game.Types.Pos
    | Undo
    | Redo
    | UpdateSetup Menu.Setup
    | NewGame Menu.Setup
    | Noop


renderScreen : Game -> Html.Html Msg
renderScreen game =
    let
        currentBoard =
            Zipper.current game.history

        nextPlayer =
            Game.Util.nextPlayer currentBoard game.handicap
    in
        div []
            [ span [] [ text ("Current Player: " ++ playerToString nextPlayer) ]
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


stringifyApp : App -> String
stringifyApp { game, setup } =
    let
        url =
            Url.root
                |> Url.append Url.hash
                |> Url.append ( Url.s <| Coding.encode game Coding.List )
    in
        Url.toString game url


update : Msg -> App -> ( App, Cmd Msg )
update msg app =
    case msg of
        OnEnter pos ->
            ( { app | game = Game.enterTile app.game pos }
            , Cmd.none
            )

        OnLeave ->
            ( { app | game = Game.leaveBoard app.game }
            , Cmd.none
            )

        OnClick pos ->
            let
                app_ = { app | game = Game.clickTile app.game pos }
            in
                ( app_
                , Navigation.modifyUrl <| stringifyApp app_
                )

        Undo ->
            ( { app | game = Game.undo app.game }
            , Cmd.none
            )

        Redo ->
            ( { app | game = Game.redo app.game }
            , Cmd.none
            )

        UpdateSetup setup ->
            ( { app | setup = setup }
            , Cmd.none
            )

        NewGame setup ->
            let
                app_ = { app | game = Maybe.withDefault app.game (createGame setup) }
            in
                ( app_
                , Navigation.modifyUrl <| stringifyApp app_
                )

        Noop ->
            ( app, Cmd.none )


main : Program Never App Msg
main =
    Navigation.program
        ( \_ -> Noop )
        { init = newApp
        , view = render
        , update = update
        , subscriptions = \_ -> Sub.none
        }
