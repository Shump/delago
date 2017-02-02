module View exposing (render)

import Result
import String as S
import Maybe
import Maybe.Extra exposing (maybeToList)
import Html exposing (div, span, text, input, button)
import Html.App exposing (map)
import Html.Events exposing (onClick, onInput)
import Model
import Msg
import View.Board exposing (renderBoard)


render : Model.App -> Html.Html Msg.Msg
render app =
    let
        getInt tag str =
            S.toInt str
                |> Result.map (tag << Just)
                |> Result.withDefault (tag Nothing)

        -- getFloat tag str =
        --   S.toFloat str
        --     |> Result.map tag
        --     |> Result.withDefault (tag Nothing)
    in
        case app of
            Model.NewGame setup ->
                let
                    sizeElements =
                        [ text "Board size: "
                        , input [ onInput Msg.Size ] []
                        ]
                            ++ (List.map text <| maybeToList setup.errors.size)

                    komiElements =
                        [ text "Komi: "
                        , input [ onInput Msg.Komi ] []
                        ]
                            ++ (List.map text <| maybeToList setup.errors.komi)

                    handicapElements =
                        [ text "Handicap: "
                        , input [ onInput Msg.Handicap ] []
                        ]
                            ++ (List.map text <| maybeToList setup.errors.handicap)
                in
                    div []
                        [ text "New game"
                        , div [] sizeElements
                        , div [] komiElements
                        , div [] handicapElements
                        , button [ onClick Msg.Start ] [ text "Start!" ]
                        ]

            Model.Game game ->
                map (\msg -> Msg.Game msg) <| renderBoard game
