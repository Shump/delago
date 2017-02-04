module Menu exposing (..)

import Html exposing (div, fieldset, label, text, input, button)
import Html.Attributes exposing (type_, name)
import Html.Events exposing (onClick, onInput)


type BoardSize
    = Nine
    | Thirtheen
    | Nineteen


type alias Komi =
    Float


type alias Handicap =
    Int


type alias Setup =
    { size : BoardSize
    , komi : Komi
    , handicap : Handicap
    }


newSetup : Setup
newSetup =
    { size = Nineteen
    , komi = 0
    , handicap = 0
    }


type Msg
    = Update Setup


render : Setup -> Html.Html Msg
render setup =
    div []
        [ fieldset []
            [ label []
                [ input [ type_ "radio", name "board-size", onClick (Update setup) ] []
                , text "9x9"
                ]
            , label []
                [ input [ type_ "radio", name "board-size", onClick (Update setup) ] []
                , text "13x13"
                ]
            , label []
                [ input [ type_ "radio", name "board-size", onClick (Update setup) ] []
                , text "19x19"
                ]
            ]
        ]
