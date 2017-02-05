module Menu exposing (..)

import String
import Maybe exposing (Maybe(..))
import Result
import Html exposing (div, fieldset, label, text, input, button)
import Html.Attributes as Attr exposing (type_, name, checked, disabled)
import Html.Events exposing (onClick, onInput)
import Util exposing (isCycleOf)


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
    , komi : Maybe Komi
    , okigo : Maybe Okigo
    }


type alias DefaultSetup =
    { size : BoardSize
    , komi : Komi
    , okigo : Okigo
    }


newSetup : BoardSize -> Komi -> Okigo -> Setup
newSetup size komi okigo =
    { size = size
    , komi = Just komi
    , okigo = Just okigo
    }


updateSize : Setup -> BoardSize -> Setup
updateSize setup size =
    { setup | size = size }


updateKomi : Setup -> Maybe Komi -> Setup
updateKomi setup komi =
    { setup | komi = komi }


updateOkigo : Setup -> Maybe Okigo -> Setup
updateOkigo setup okigo =
    { setup | okigo = okigo }


isSetupValid : Setup -> Bool
isSetupValid { komi, okigo } =
    let
        verify komi_ okigo_ =
            komi_ >= 0 && isCycleOf komi_ 0.5 && okigo_ >= 0
    in
        Maybe.withDefault False <| Maybe.map2 verify komi okigo


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


render : DefaultSetup -> Setup -> Html.Html Msg
render defaults setup =
    let
        radio_ =
            radio defaults.size setup

        komiUpdate_ =
            Update << updateKomi setup

        updateKomi_ str =
            Result.withDefault (komiUpdate_ Nothing) <|
                Result.map (komiUpdate_ << Just) (String.toFloat str)

        okigoUpdate_ =
            Update << updateOkigo setup

        updateOkigo_ str =
            Result.withDefault (okigoUpdate_ Nothing) <|
                Result.map (okigoUpdate_ << Just) (String.toInt str)

        isSubmitDisabled_ =
            not <| isSetupValid setup
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
            , button
                [ onClick NewGame
                , disabled isSubmitDisabled_
                ]
                [ text "New Game" ]
            ]
