module View.Shared exposing (Callbacks)


type alias Callbacks msg =
    { onEnter : { x : Int, y : Int } -> msg
    , onLeave : msg
    , onClick : { x : Int, y : Int } -> msg
    }
