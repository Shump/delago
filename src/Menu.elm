module Menu exposing (..)

import Html
import Html.Attributes
import Html.Events
import Maybe exposing (Maybe(..))
import Result
import String

import Element
import Element.Input as Input
import Element.Attributes as Attributes
import Element.Events as Events
import Style


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


newSetup : BoardSize -> Okigo -> Setup
newSetup size okigo =
    { size = size
    , okigo = Just okigo
    }


isSetupValid : Setup -> Bool
isSetupValid { okigo } =
    let
        verify okigo_ =
            okigo_ >= 0
    in
        Maybe.withDefault False <| Maybe.map verify okigo


type Style
    = NoStyle


stylesheet : Style.StyleSheet Style variation
stylesheet =
    Style.styleSheet
        [ Style.style NoStyle []
        ]


render : Setup -> { update : Setup -> a, newGame : Setup -> a } -> Html.Html a
render setup msgs =
    let
        radio setup update =
            let
                choice value =
                    let
                        size = toString (sizeToInt value)
                    in
                        Input.choice value (Element.text <| size ++ "x" ++ size)
            in
                Input.radioRow NoStyle []
                    { onChange = (\value -> (update { setup | size = value }))
                    , selected = Just setup.size
                    , label = Input.labelLeft (Element.text "Board Size:")
                    , options = []
                    , choices =
                        [ choice Nine
                        , choice Thirteen
                        , choice Nineteen
                        ]
                    }

        updateOkigo_ str =
            msgs.update { setup | okigo = Result.toMaybe (String.toInt str) }

        okigo =
            Element.row NoStyle []
                [ Element.text "Handicap stones:"
                , Element.node "input" <| Element.el NoStyle
                    [ Attributes.attribute "type" "number"
                    , Attributes.attribute "min" "0"
                    , Attributes.attribute "step" "1"
                    , Attributes.attribute "value" (toString <| Maybe.withDefault 0 setup.okigo)
                    , Events.onInput updateOkigo_
                    ]
                    (Element.text "test")
                ]

        submit =
            Element.html <| Html.button
                [ Html.Events.onClick <| msgs.newGame setup
                , Html.Attributes.disabled <| not (isSetupValid setup)
                ]
                [ Html.text "New Game" ]
    in
        Element.layout stylesheet <|
            Element.column NoStyle []
            [ radio setup msgs.update
            , okigo
            , submit
            ]
