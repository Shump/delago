module Model exposing (..)

import Result exposing (Result)
import Maybe exposing (Maybe)

import Model.Game as M

type alias Errors = 
  { size : Maybe String
  , komi : Maybe String
  , handicap : Maybe String
  }

type alias Setup =
  { size : Maybe Int
  , komi : Maybe Float
  , handicap : Maybe Int
  , errors : Errors
  }

type App
  = NewGame Setup
  | Game M.Game

updateSetup : (Setup -> Setup) -> App -> App
updateSetup func app =
  case app of
    NewGame setup -> NewGame <| func setup
    _ -> app

updateErrors : (Errors -> Errors) -> App -> App
updateErrors func app =
  case app of
    NewGame setup ->
      let
        errors_ = setup.errors
      in
        NewGame <| { setup | errors = func errors_ }
    _ -> app

validateSize : Int -> Result String Int
validateSize size =
  if size > 0 then
    Ok size
  else
    Err "Size has to be greater than 0"

validateKomi : Float -> Result String Float
validateKomi komi =
  if komi >= 0 then
    Ok komi
  else
    Err "Komi has to be a positive number"

validateHandicap : Int -> Result String Int
validateHandicap handicap =
  if handicap >= 0 then
    Ok handicap
  else
    Err "Handicap has to be a positive integer"

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
    , errors =
      { size = Nothing
      , komi = Nothing
      , handicap = Nothing
      }
    }
