module Game.Types exposing (Stone(..) , Pos , Player, isBlack, isWhite)


type Stone
    = Black
    | White


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Player =
    Stone


isBlack : Stone -> Bool
isBlack stone =
    stone == Black


isWhite : Stone -> Bool
isWhite stone =
    stone == White
