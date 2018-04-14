module Game.View exposing (renderBoard)

import Html
import Maybe
import Dict
import List
import Svg exposing (Svg, svg, use)
import Svg.Events as Events
import Svg.Attributes as Attributes exposing (xlinkHref, x, y, width, height)
import List.Zipper as Zipper
import Game.Types
    exposing
        ( Game
        , Pos
        , Point(..)
        , Stone(..)
        , Player
        )
import Game.SvgDefs
    exposing
        ( viewBox_
        , x_
        , y_
        , width_
        , height_
        , boardSymbol
        )


renderBlackStone : Pos -> { onEnter : Pos -> a, onLeave : a, onClick : Pos -> a } -> Svg a
renderBlackStone ({ x, y } as pos) msgs =
    Svg.rect
        [ x_ x
        , y_ y
        , width_ 1
        , height_ 1
        , Attributes.fill "black"
        , Events.onMouseUp <| msgs.onClick pos
        , Events.onMouseOver <| msgs.onEnter pos
        ]
        []


renderWhiteStone : Pos -> { onEnter : Pos -> a, onLeave : a, onClick : Pos -> a } -> Svg a
renderWhiteStone ({ x, y } as pos) msgs =
    Svg.rect
        [ x_ x
        , y_ y
        , width_ 1
        , height_ 1
        , Attributes.fill "white"
        , Events.onMouseUp <| msgs.onClick pos
        , Events.onMouseOver <| msgs.onEnter pos
        ]
        []


renderStone : Pos -> Player -> { onEnter : Pos -> a, onLeave : a, onClick : Pos -> a } -> Svg a
renderStone pos player msgs =
    case player of
        Black ->
            renderBlackStone pos msgs

        White ->
            renderWhiteStone pos msgs


renderEmptyTile : Pos -> { onEnter : Pos -> a, onLeave : a, onClick : Pos -> a } -> Svg a
renderEmptyTile ({ x, y } as pos) msgs =
    Svg.rect
        [ x_ x
        , y_ y
        , width_ 1
        , height_ 1
        , Attributes.fill "transparent"
        , Events.onMouseUp <| msgs.onClick pos
        , Events.onMouseOver <| msgs.onEnter pos
        ]
        []


tile : Game -> { onEnter : Pos -> a, onLeave : a, onClick : Pos -> a } -> ( ( Int, Int ), Point ) -> Svg a
tile game msgs ( ( x, y ), point ) =
    let
        pos_ =
            { x = x, y = y }

        currentState =
            Zipper.current game.history
    in
        case ( point, game.hovering ) of
            ( Occupied Black, _ ) ->
                renderBlackStone pos_ msgs

            ( Occupied White, _ ) ->
                renderWhiteStone pos_ msgs

            ( Empty, Just hoveringPos ) ->
                if hoveringPos == pos_ then
                    renderStone pos_ currentState.nextPlayer msgs
                else
                    renderEmptyTile pos_ msgs

            ( Empty, Nothing ) ->
                renderEmptyTile pos_ msgs


tiles : Game.Types.Game -> { onEnter : Pos -> a, onLeave : a, onClick : Pos -> a } -> List (Svg a)
tiles game msgs =
    let
        tile_ =
            tile game msgs

        currentState =
            Zipper.current game.history
    in
        List.map tile_ <| Dict.toList currentState.board


board : Int -> { onEnter : Pos -> a, onLeave : a, onClick : Pos -> a } -> Svg a
board size msgs =
    use
        [ x_ 0
        , y_ 0
        , width_ size
        , width_ size
        , xlinkHref "#board"
        , Events.onMouseOut <| msgs.onLeave
        ]
        []


renderBoard : Game.Types.Game -> { onEnter : Pos -> a, onLeave : a, onClick : Pos -> a } -> Html.Html a
renderBoard game a =
    let
        attributes =
            [ width_ 512
            , height_ 512
            , viewBox_ 0 0 game.size game.size
            ]

        subElems =
            [ boardSymbol game.size
            , board game.size a
            ]
                ++ tiles game a
    in
        svg attributes subElems
