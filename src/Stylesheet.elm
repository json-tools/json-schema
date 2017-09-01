module StyleSheet exposing (stylesheet, Styles(..), Variations(..))

import Style exposing (style, StyleSheet)
import Color
import Style.Color as Color
import Style.Font as Font
import Style.Border as Border


type Styles
    = None
    | Main
    | Bordered
    | Error
    | Button
    | MenuItem
    | HtmlAttribute
    | PropertyName
    | PropertySeparator
    | PropertyValue
    | HtmlElement


type Variations
    = Active


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.stylesheet
        [ style None []
          -- It's handy to have a blank style
        , style Main
            [ Color.text Color.darkCharcoal
            , Color.background Color.white
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Font.size 14
            , Style.paddingHint 10
            , Font.lineHeight 1.5
              -- line height, given as a ratio of current font size.
            ]
        , style Bordered
            [ Border.all 1
            , Color.border <| Color.rgb 230 230 230
            , Style.paddingHint 10
            ]
        , style Error
            [ Color.text Color.darkBrown
            , Color.background Color.white
            , Color.border Color.brown
            , Border.all 3
            , Border.rounded 3
            , Font.size 16
            , Style.paddingHint 5
            , Style.prop "margin" "10px"
            ]
        , style HtmlAttribute
            [ Color.text Color.darkBlue
              --, Style.paddingLeftHint 5
            ]
        , style PropertyName
            [ Color.text Color.darkGreen
            ]
        , style PropertySeparator
            [ Color.text Color.black
            , Style.paddingRightHint 5
            ]
        , style PropertyValue
            [ Color.text Color.darkOrange
            ]
        , style HtmlElement
            [ Font.typeface [ "menlo", "monospace" ]
            , Font.size 10
            , Color.text Color.blue
            ]
        , style Button
            [ Font.bold
            , Font.typeface [ "menlo", "monospace" ]
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Border.all 3
            , Border.solid
            , Border.rounded 3
            , Color.border Color.lightGrey
            , Style.paddingLeftHint 4
            , Style.paddingRightHint 4
            , Style.paddingTopHint 3
            , Style.paddingBottomHint 3
            ]
        , style MenuItem
            [ Style.paddingHint 5
            , Color.text Color.blue
            , Font.underline
            , Style.cursor "pointer"
            , Style.variation Active
                [ Color.text Color.black
                , Style.prop "text-decoration" "none"
                , Style.cursor "default"
                ]
            ]
        ]
