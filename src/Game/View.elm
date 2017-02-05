module Game.View exposing (renderBoard)

import Html
import Maybe exposing (map)
import Maybe.Extra exposing (filter, or)
import Dict
import List
import Svg exposing (Svg, svg, use)
import Svg.Events as Events
import Svg.Attributes exposing (xlinkHref, x, y, width, height)
import List.Zipper as Zipper
import Game.Model
    exposing
        ( Game
        , Pos
        , Point(Empty, Occupied)
        , Stone(Black, White)
        )
import Game.Msg
import Game.SvgDefs
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


tile : Game -> ( ( Int, Int ), Point ) -> Svg Game.Msg.Msg
tile game ( ( x, y ), point ) =
    let
        pos_ =
            { x = x, y = y }

        isHovering =
            (==) pos_

        currentState =
            Zipper.current game.history

        nextTileId =
            stoneId currentState.nextPlayer

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
            , Events.onClick <| Game.Msg.OnClick pos_
            , Events.onMouseOver <| Game.Msg.OnEnter pos_
            ]
            []


tiles : Game.Model.Game -> List (Svg Game.Msg.Msg)
tiles game =
    let
        tile_ =
            tile game

        currentState =
            Zipper.current game.history
    in
        List.map tile_ <| Dict.toList currentState.board


board : Int -> Svg Game.Msg.Msg
board size =
    use
        [ x_ 0
        , y_ 0
        , width_ size
        , width_ size
        , xlinkHref "#board"
        , Events.onMouseOut <| Game.Msg.OnLeave
        ]
        []


renderBoard : Game.Model.Game -> Html.Html Game.Msg.Msg
renderBoard game =
    let
        attributes =
            [ width_ 512
            , height_ 512
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
