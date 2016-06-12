module Util exposing (..)

import List.Extra exposing (andThen)

cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct xs ys =
  xs `andThen` \x ->
    ys `andThen` \y ->
      [(x, y)]
