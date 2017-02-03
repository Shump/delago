module Update exposing (update)

import String as S
import Result as R exposing (Result)
import Maybe exposing (Maybe)
import Model
    exposing
        ( updateGame
        , updateSetup
        , updateErrors
        , validateSize
        , validateKomi
        , validateHandicap
        )
import Game.Model
import Game.Update as G
import Msg


doStart : Model.App -> Model.App
doStart app =
    case app of
        Model.NewGame setup ->
            Model.Game <| Game.Model.newGame (Maybe.withDefault 19 setup.size) 0 0

        Model.Game _ ->
            app


doSize : Model.App -> String -> Model.App
doSize app size =
    let
        validatedSize =
            (S.toInt size) |> R.andThen validateSize
    in
        case validatedSize of
            Ok size_ ->
                app
                    |> updateSetup (\setup -> { setup | size = Just size_ })
                    |> updateErrors (\errors -> { errors | size = Nothing })

            Err errs ->
                updateErrors (\errors -> { errors | size = Just errs }) app


doKomi : Model.App -> String -> Model.App
doKomi app komi =
    let
        validated =
            (S.toFloat komi) |> R.andThen validateKomi
    in
        case validated of
            Ok komi_ ->
                app
                    |> updateSetup (\setup -> { setup | komi = Just komi_ })
                    |> updateErrors (\errors -> { errors | komi = Nothing })

            Err errs ->
                updateErrors (\errors -> { errors | komi = Just errs }) app


doHandicap : Model.App -> String -> Model.App
doHandicap app handicap =
    let
        validated =
            (S.toInt handicap) |> R.andThen validateHandicap
    in
        case validated of
            Ok handicap_ ->
                app
                    |> updateSetup (\setup -> { setup | handicap = Just handicap_ })
                    |> updateErrors (\errors -> { errors | handicap = Nothing })

            Err errs ->
                updateErrors (\errors -> { errors | handicap = Just errs }) app


update : Msg.Msg -> Model.App -> Model.App
update msg app =
    case msg of
        Msg.Start ->
            doStart app

        Msg.Size size ->
            doSize app size

        Msg.Komi komi ->
            doKomi app komi

        Msg.Handicap handicap ->
            doHandicap app handicap

        Msg.Game msg_ ->
            updateGame (G.update msg_) app
