module Menu exposing (..)

import String
import Result
import Html exposing (div, fieldset, label, text, input, button)
import Html.Attributes as Attr exposing (type_, name, checked)
import Html.Events exposing (onClick, onInput)


type BoardSize
    = Nine
    | Thirteen
    | Nineteen


sizeToInt : BoardSize -> Int
sizeToInt size =
    case size of
        Nine ->
            9

        Thirteen ->
            13

        Nineteen ->
            19


type alias Komi =
    Float


type alias Okigo =
    Int


type alias Setup =
    { size : BoardSize
    , komi : Komi
    , okigo : Okigo
    }


newSetup : BoardSize -> Komi -> Okigo -> Setup
newSetup size komi okigo =
    { size = size
    , komi = komi
    , okigo = okigo
    }


updateSize : Setup -> BoardSize -> Setup
updateSize setup size =
    { setup | size = size }


updateKomi : Setup -> Komi -> Setup
updateKomi setup komi =
    { setup | komi = komi }


updateOkigo : Setup -> Okigo -> Setup
updateOkigo setup okigo =
    { setup | okigo = okigo }


type Msg
    = Update Setup
    | NewGame


radio : BoardSize -> Setup -> BoardSize -> String -> Html.Html Msg
radio default setup value title =
    label []
        [ input
            [ type_ "radio"
            , name "board-size"
            , onClick (Update { setup | size = value })
            , checked (default == value)
            ]
            []
        , text title
        ]


render : Setup -> Setup -> Html.Html Msg
render defaults setup =
    let
        radio_ =
            radio defaults.size setup

        updateKomi_ str =
            Result.withDefault (Update setup) <|
                Result.map (Update << updateKomi setup) (String.toFloat str)

        updateOkigo_ str =
            Result.withDefault (Update setup) <|
                Result.map (Update << updateOkigo setup) (String.toInt str)
    in
        div []
            [ fieldset []
                [ label [] [ text "Board size:" ]
                , radio_ Nine "9x9"
                , radio_ Thirteen "13x13"
                , radio_ Nineteen "19x19"
                ]
            , label []
                [ text "Komi:"
                , input
                    [ type_ "number"
                    , Attr.min "0.5"
                    , Attr.step "0.5"
                    , Attr.defaultValue (toString defaults.komi)
                    , onInput updateKomi_
                    ]
                    []
                ]
            , label []
                [ text "Handicap stones:"
                , input
                    [ type_ "number"
                    , Attr.min "0"
                    , Attr.step "1"
                    , Attr.defaultValue (toString defaults.okigo)
                    , onInput updateOkigo_
                    ]
                    []
                ]
            , button [ onClick NewGame ] [ text "New Game" ]
            ]
