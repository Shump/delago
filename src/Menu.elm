module Menu exposing (..)

import Html exposing (div, fieldset, label, text, input, button)
import Html.Attributes as Attr exposing (type_, name, checked, disabled)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (Maybe(..))
import Result
import String


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


type alias Okigo =
    Int


type alias Setup =
    { size : BoardSize
    , okigo : Maybe Okigo
    }


type alias DefaultSetup =
    { size : BoardSize
    , okigo : Okigo
    }


newSetup : BoardSize -> Okigo -> Setup
newSetup size okigo =
    { size = size
    , okigo = Just okigo
    }


updateSize : Setup -> BoardSize -> Setup
updateSize setup size =
    { setup | size = size }


updateOkigo : Setup -> Maybe Okigo -> Setup
updateOkigo setup okigo =
    { setup | okigo = okigo }


isSetupValid : Setup -> Bool
isSetupValid { okigo } =
    let
        verify okigo_ =
            okigo_ >= 0
    in
        Maybe.withDefault False <| Maybe.map verify okigo


radio : BoardSize -> Setup -> ( Setup -> a ) -> BoardSize -> String -> Html.Html a
radio default setup update value title =
    label []
        [ input
            [ type_ "radio"
            , name "board-size"
            , onClick (update { setup | size = value })
            , checked (default == value)
            ]
            []
        , text title
        ]


render : DefaultSetup -> Setup -> { update : Setup -> a, newGame : Setup -> a } -> Html.Html a
render defaults setup msgs =
    let
        radio_ =
            radio defaults.size setup msgs.update

        okigoUpdate_ =
            msgs.update << updateOkigo setup

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
                [ onClick <| msgs.newGame setup
                , disabled isSubmitDisabled_
                ]
                [ text "New Game" ]
            ]
