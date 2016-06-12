module View exposing (render)

import Json.Decode exposing (succeed, object2, (:=), float)

import Html
import Maybe exposing (map)
import Maybe.Extra exposing (filter, or)
import Dict
import List

import Svg exposing (Svg, svg, use)
import Svg.Events as Events
import Svg.Attributes exposing (xlinkHref, x, y, width, height)

import Shared exposing
  ( Game
  , Pos
  , Point(Empty, Occupied)
  , Stone(Black, White)
  )
import SvgDefs exposing
  ( viewBox_
  , x_, y_
  , width_, height_
  , emptyTileSymbol, emptyId
  , blackTileSymbol, blackId
  , whiteTileSymbol, whiteId
  , boardSymbol
  )

type alias Callbacks msg =
  { onEnter : { x : Int, y : Int } -> msg
  , onLeave : msg
  , onClick : { x : Int, y : Int } -> msg
  }

stoneId : Stone -> String
stoneId stone =
  case stone of
    Black -> blackId
    White -> whiteId

pointId : Point -> String
pointId point =
  case point of
    Empty -> emptyId
    Occupied stone -> stoneId stone

tile : Callbacks msg -> Game -> ((Int, Int), Point) -> Svg msg
tile callbacks game ((x, y), point) =
  let
    pos_ = { x = x, y = y }

    isHovering = (==) pos_

    nextTileId = stoneId game.nextPlayer

    boardId =
      case point of
        Empty ->
          Nothing
        Occupied stone ->
          Just <| stoneId stone

    hoveringId =
      filter isHovering game.hovering
        |> map (always nextTileId)

    sym = or boardId hoveringId

  in
    use
      [ x_ x , y_ y
      , width_ 1 , height_ 1
      , xlinkHref <| Maybe.withDefault emptyId sym
      , Events.onClick <| callbacks.onClick pos_
      , Events.onMouseOver <| callbacks.onEnter pos_
      ] []

tiles : Callbacks msg -> Shared.Game -> List (Svg msg)
tiles callbacks game =
  let
    tile_ = tile callbacks game
  in
    List.map tile_ <| Dict.toList game.board

board : Callbacks msg -> Int -> Svg msg
board callbacks size =
  use
    [ x_ 0, y_ 0
    , width_ size , width_ size
    , xlinkHref "#board"
    , Events.onMouseOut <| callbacks.onLeave
    ]
      []

render : Callbacks msg -> Shared.Game -> Html.Html msg
render callbacks game =
  let
    attributes =
      [ width_ 600 , height_ 600
      , viewBox_ 0 0 game.size game.size
      ]

    subElems =
      [ boardSymbol game.size
      , emptyTileSymbol
      , blackTileSymbol
      , whiteTileSymbol
      , board callbacks game.size
      ] ++ tiles callbacks game

  in
    svg attributes subElems
