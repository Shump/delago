module List.Zipper.Extra exposing (..)

import Maybe
import List.Zipper as Zipper exposing (Zipper(..))


appendRight : List a -> Zipper a -> Zipper a
appendRight xs (Zipper ls x rs) =
    Zipper ls x (rs ++ xs)


addRight : a -> Zipper a -> Zipper a
addRight x zipper =
    appendRight [ x ] zipper


replaceRight : List a -> Zipper a -> Zipper a
replaceRight xs (Zipper ls x rs) =
    Zipper ls x xs


next_ : Zipper a -> Zipper a
next_ zipper =
    Maybe.withDefault zipper <| Zipper.next zipper


previous_ : Zipper a -> Zipper a
previous_ zipper =
    Maybe.withDefault zipper <| Zipper.previous zipper
