module View.SvgDefs exposing
  ( viewBox_
  , x_, y_
  , width_, height_
  , emptyTileSymbol, emptyId
  , blackTileSymbol, blackId
  , whiteTileSymbol, whiteId
  , boardSymbol
  )

import List
import String

import Svg exposing (Svg)
import Svg.Attributes as A

viewBox_ : Int -> Int -> Int -> Int -> Svg.Attribute msg
viewBox_ x1_ y1_ x2_ y2_ =
  A.viewBox <| String.join " " <| List.map toString [x1_, y1_, x2_, y2_]

x_ : Int -> Svg.Attribute msg
x_ value = A.x <| toString value

y_ : Int -> Svg.Attribute msg
y_ value = A.y <| toString value

width_ : Int -> Svg.Attribute msg
width_ value = A.width <| toString value

height_ : Int -> Svg.Attribute msg
height_ value = A.height <| toString value

emptyId : String
emptyId = "#emptyTile"

emptyTileSymbol : Svg msg
emptyTileSymbol =
  Svg.symbol
    [ A.id "emptyTile", A.viewBox "0 0 1 1" ]
    [ Svg.rect
      [ A.x "0"
      , A.y "0"
      , A.width "1"
      , A.height "1"
      , A.opacity "0"
      ]
      []
    ]

blackId : String
blackId = "#blackTile"

blackTileSymbol : Svg msg
blackTileSymbol =
  Svg.symbol
    [ A.id "blackTile", A.viewBox "0 0 1 1" ]
    [ Svg.rect
      [ A.x "0"
      , A.y "0"
      , A.width "1"
      , A.height "1"
      , A.fill "black"
      ]
      []
    ]

whiteId : String
whiteId = "#whiteTile"

whiteTileSymbol : Svg msg
whiteTileSymbol =
  Svg.symbol
    [ A.id "whiteTile", A.viewBox "0 0 1 1" ]
    [ Svg.rect
      [ A.x "0"
      , A.y "0"
      , A.width "1"
      , A.height "1"
      , A.fill "white"
      ]
      []
    ]

supportPositions : Int -> List (Int, Int)
supportPositions size =
  case size of
    9 ->
      [ (2, 2), (6, 2)
      , (4, 4)
      , (2, 6), (6, 6)
      ]
    13 ->
      [ (3, 3), (9, 3)
      , (6, 6)
      , (3, 9), (9, 9)
      ]
    19 ->
      [ (3, 3), (9, 3), (15, 3)
      , (3, 9), (9, 9), (15, 9)
      , (3, 15), (9, 15), (15, 15)
      ]
    _ -> []

lineColor : String
lineColor = "black"

boardColor : String
boardColor = "orange"

boardSymbol : Int -> Svg msg
boardSymbol size =
  let
    boardViewBox size = viewBox_ 0 0 (size * 2) (size * 2)

    vline i =
      Svg.line
        [ A.x1 <| toString (i * 2 + 3)
        , A.y1 "1"
        , A.x2 <| toString (i * 2 + 3)
        , A.y2 <| toString (size * 2 - 1)
        , A.stroke lineColor
        , A.strokeWidth "0.1"
        ]
        []

    hline i =
      Svg.line
        [ A.y1 <| toString (i * 2 + 3)
        , A.x1 "1"
        , A.y2 <| toString (i * 2 + 3)
        , A.x2 <| toString (size * 2 - 1)
        , A.stroke lineColor
        , A.strokeWidth "0.1"
        ]
        []

    supportPos (x, y) =
      Svg.circle
        [ A.cx <| toString (x * 2 + 1)
        , A.cy <| toString (y * 2 + 1)
        , A.r "0.2"
        , A.fill lineColor
        ]
        []

  in
    Svg.symbol
      [ A.id "board", boardViewBox size ] <|
      [ Svg.rect
          [ A.x "0"
          , A.y "0"
          , A.width <| toString (size * 2)
          , A.height <| toString (size * 2)
          , A.fill boardColor
          ]
          []
      , Svg.rect
          [ A.x "1"
          , A.y "1"
          , A.width <| toString (size * 2 - 2)
          , A.height <| toString (size * 2 - 2)
          , A.stroke lineColor
          , A.strokeWidth "0.1"
          , A.fill "transparent"
          ]
          []
      ] ++ List.map vline [0..size - 3]
        ++ List.map hline [0..size - 3]
        ++ List.map supportPos (supportPositions size)
