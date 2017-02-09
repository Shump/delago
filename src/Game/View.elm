module Game.View exposing (renderBoard)

import Html
import Maybe
import Dict
import List
import Svg exposing (Svg, svg, use)
import Svg.Events as Events
import Svg.Attributes as Attributes exposing (xlinkHref, x, y, width, height)
import List.Zipper as Zipper
import Game.Model
    exposing
        ( Game
        , Pos
        , Point(..)
        , Stone(..)
        , Player
        )
import Game.Msg
import Game.SvgDefs
    exposing
        ( viewBox_
        , x_
        , y_
        , width_
        , height_
        , boardSymbol
        )


renderBlackStone : Pos -> Svg Game.Msg.Msg
renderBlackStone ({ x, y } as pos) =
    Svg.rect
        [ x_ x
        , y_ y
        , width_ 1
        , height_ 1
        , Attributes.fill "black"
        , Events.onMouseUp <| Game.Msg.OnClick pos
        , Events.onMouseOver <| Game.Msg.OnEnter pos
        ]
        []


renderWhiteStone : Pos -> Svg Game.Msg.Msg
renderWhiteStone ({ x, y } as pos) =
    Svg.rect
        [ x_ x
        , y_ y
        , width_ 1
        , height_ 1
        , Attributes.fill "white"
        , Events.onMouseUp <| Game.Msg.OnClick pos
        , Events.onMouseOver <| Game.Msg.OnEnter pos
        ]
        []


renderStone : Pos -> Player -> Svg Game.Msg.Msg
renderStone pos player =
    case player of
        Black ->
            renderBlackStone pos

        White ->
            renderWhiteStone pos


renderEmptyTile : Pos -> Svg Game.Msg.Msg
renderEmptyTile ({ x, y } as pos) =
    Svg.rect
        [ x_ x
        , y_ y
        , width_ 1
        , height_ 1
        , Attributes.fill "transparent"
        , Events.onMouseUp <| Game.Msg.OnClick pos
        , Events.onMouseOver <| Game.Msg.OnEnter pos
        ]
        []


tile : Game -> ( ( Int, Int ), Point ) -> Svg Game.Msg.Msg
tile game ( ( x, y ), point ) =
    let
        pos_ =
            { x = x, y = y }

        currentState =
            Zipper.current game.history
    in
        case ( point, game.hovering ) of
            ( Occupied Black, _ ) ->
                renderBlackStone pos_

            ( Occupied White, _ ) ->
                renderWhiteStone pos_

            ( Empty, Just hoveringPos ) ->
                if hoveringPos == pos_ then
                    renderStone pos_ currentState.nextPlayer
                else
                    renderEmptyTile pos_

            ( Empty, Nothing ) ->
                renderEmptyTile pos_


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
            , board game.size
            ]
                ++ tiles game
    in
        svg attributes subElems
