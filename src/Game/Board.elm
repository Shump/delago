module Game.Board
    exposing
        ( Board
        , newBoard
        , putPoint
        , removePoint
        , getPoint
        , stonesCount
        , stones
        , blackStonesCount
        , blackStones
        , whiteStonesCount
        , whiteStones
        , taken
        , blackTaken
        , whiteTaken
        )

import Dict
import Maybe

import Maybe.Extra

import Game.Types exposing (..)


type alias Size = Int


type alias Repr =
    { board : Dict.Dict ( Int, Int ) Stone
    , blackTaken : Int
    , whiteTaken : Int
    }


type Board = Board Repr


newBoard : Size -> Board
newBoard _ =
    Board
        { board = Dict.empty
        , blackTaken = 0
        , whiteTaken = 0
        }


putPoint : Pos -> Stone -> Board -> Board
putPoint { x, y } stone (Board repr) =
    Board { repr | board = Dict.update ( x, y ) ( Maybe.Extra.or <| Just stone ) repr.board }


removePoint : Pos -> Board -> Board
removePoint { x, y } ((Board repr) as board) =
    let
        take repr stone =
            if stone == Black then
                { repr |
                    board = Dict.remove ( x, y ) repr.board,
                    blackTaken = repr.blackTaken + 1
                }
            else
                { repr |
                    board = Dict.remove ( x, y ) repr.board,
                    whiteTaken = repr.whiteTaken + 1
                }
    in
        Dict.get ( x, y ) repr.board
            |> Maybe.map (Board << (take repr))
            |> Maybe.withDefault board


getPoint : Board -> Pos -> Maybe Stone
getPoint (Board repr) { x, y } =
    Dict.get ( x, y ) repr.board


stonesCount : Board -> Int
stonesCount (Board repr) =
    Dict.size repr.board


stones : Board -> List ( ( Int, Int ), Stone )
stones (Board repr) =
    Dict.toList repr.board


blackStonesCount : Board -> Int
blackStonesCount (Board repr) =
    Dict.foldl (\_ stone count -> if isBlack stone then count + 1 else count) 0 repr.board


blackStones : Board -> List ( (Int, Int ), Stone )
blackStones (Board repr) =
    Dict.foldl (\pos stone list -> if isBlack stone then ( pos, stone ) :: list else list) [] repr.board


whiteStonesCount : Board -> Int
whiteStonesCount (Board repr) =
    Dict.foldl (\_ stone count -> if isWhite stone then count + 1 else count) 0 repr.board


whiteStones : Board -> List ( (Int, Int ), Stone )
whiteStones (Board repr) =
    Dict.foldl (\pos stone list -> if isWhite stone then ( pos, stone ) :: list else list) [] repr.board


taken : Stone -> Board -> Int
taken stone (Board repr) =
    if stone == Black then
        repr.blackTaken
    else
        repr.whiteTaken


blackTaken : Board -> Int
blackTaken (Board repr) =
    repr.blackTaken


whiteTaken : Board -> Int
whiteTaken (Board repr) =
    repr.whiteTaken
