module Model exposing (..)

import Maybe exposing (Maybe)

import Model.Game as M

type alias Setup =
  { size : Maybe Int
  , komi : Maybe Float
  , handicap : Maybe Int
  }

type App
  = NewGame Setup
  | Game M.Game

updateSetup : (Setup -> Setup) -> App -> App
updateSetup func app =
  case app of
    NewGame setup -> NewGame <| func setup
    _ -> app

updateGame : (M.Game -> M.Game) -> App -> App
updateGame func app =
  case app of
    Game game -> Game <| func game
    _ -> app

newApp : App
newApp =
  NewGame
    { size = Nothing
    , komi = Nothing
    , handicap = Nothing
    }

start : App -> App
start app =
  case app of
    NewGame setup -> Game <| M.newGame 19 0 0
    Game _ -> app
