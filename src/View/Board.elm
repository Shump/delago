module View.Board exposing (renderBoard)

import Html
import Maybe exposing (map)
import Maybe.Extra exposing (filter, or)
import Dict
import List
import Svg exposing (Svg, svg, use)
import Svg.Events as Events
import Svg.Attributes exposing (xlinkHref, x, y, width, height)
import Game.Model
    exposing
        ( Game
        , Pos
        , Point(Empty, Occupied)
        , Stone(Black, White)
        )
import Msg.Game
import View.SvgDefs
    exposing
        ( viewBox_
        , x_
        , y_
        , width_
        , height_
        , emptyTileSymbol
        , emptyId
        , blackTileSymbol
        , blackId
        , whiteTileSymbol
        , whiteId
        , boardSymbol
        )


stoneId : Stone -> String
stoneId stone =
    case stone of
        Black ->
            blackId

        White ->
            whiteId


pointId : Point -> String
pointId point =
    case point of
        Empty ->
            emptyId

        Occupied stone ->
            stoneId stone


tile : Game -> ( ( Int, Int ), Point ) -> Svg Msg.Game.Msg
tile game ( ( x, y ), point ) =
    let
        pos_ =
            { x = x, y = y }

        isHovering =
            (==) pos_

        nextTileId =
            stoneId game.nextPlayer

        boardId =
            case point of
                Empty ->
                    Nothing

                Occupied stone ->
                    Just <| stoneId stone

        hoveringId =
            filter isHovering game.hovering
                |> map (always nextTileId)

        sym =
            or boardId hoveringId
    in
        use
            [ x_ x
            , y_ y
            , width_ 1
            , height_ 1
            , xlinkHref <| Maybe.withDefault emptyId sym
            , Events.onClick <| Msg.Game.OnClick pos_
            , Events.onMouseOver <| Msg.Game.OnEnter pos_
            ]
            []


tiles : Game.Model.Game -> List (Svg Msg.Game.Msg)
tiles game =
    let
        tile_ =
            tile game
    in
        List.map tile_ <| Dict.toList game.board


board : Int -> Svg Msg.Game.Msg
board size =
    use
        [ x_ 0
        , y_ 0
        , width_ size
        , width_ size
        , xlinkHref "#board"
        , Events.onMouseOut <| Msg.Game.OnLeave
        ]
        []


renderBoard : Game.Model.Game -> Html.Html Msg.Game.Msg
renderBoard game =
    let
        attributes =
            [ width_ 600
            , height_ 600
            , viewBox_ 0 0 game.size game.size
            ]

        subElems =
            [ boardSymbol game.size
            , emptyTileSymbol
            , blackTileSymbol
            , whiteTileSymbol
            , board game.size
            ]
                ++ tiles game
    in
        svg attributes subElems
