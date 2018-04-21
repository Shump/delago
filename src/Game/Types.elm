module Game.Types exposing (Stone(..) , Pos , Player)


type Stone
    = Black
    | White


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Player =
    Stone
